module Python.Interpreter where

import Python.AST
import Prelude hiding (lookup)
import Control.Monad
import Control.Monad.Free
import Control.Joint
import Control.Lens (element, (%~))
import Data.Bool
import Data.Functor
import Data.Map.Strict (Map, fromList, insert, lookup)
import Text.Read (readEither)

type Scope = Map Name Value
type Callstack = [Scope]

data RuntimeError =  ValueError String
                  | OutOfScope Name
                  | ArgumentMismatch Name [Value]
                  | BinTypeMismatch Binop Value Value
                  | UnTypeMismatch Unop Value
                  | EarlyExit Value
                  deriving (Show)

type Interpreter = State Callstack :> Either RuntimeError :> IO

getValue :: (Monad m, Stateful Callstack m, Failable RuntimeError m) => Name -> m Value
getValue "print" = return $ VBuiltin Print
getValue "input" = return $ VBuiltin Input
getValue "str" = return $ VBuiltin Str
getValue "int" = return $ VBuiltin Int
getValue ident = lookup ident <$$> current @Callstack
    >>= maybe (failure @RuntimeError $ OutOfScope ident) pure . msum

setValue :: (Stateful Callstack m) => Name -> Value -> m ()
setValue ident value = modify @Callstack $ element 0 %~ insert ident value

binop :: (Monad m, Failable RuntimeError m) => Binop -> Value -> Value -> m Value
binop Add (VInt l) (VInt r) = return $ VInt $ l + r
binop Add (VString l) (VString r) = return $ VString $ l <> r
binop Sub (VInt l) (VInt r) = return $ VInt $ l - r
binop Mul (VInt l) (VInt r) = return $ VInt $ l * r
binop Div (VInt l) (VInt r) = return $ VInt $ l `div` r
binop Mod (VInt l) (VInt r) = return $ VInt $ l `mod` r
binop Eq (VInt l) (VInt r) = return $ VBool $ l == r
binop Eq (VBool l) (VBool r) = return $ VBool $ l == r
binop Eq (VString l) (VString r) = return $ VBool $ l == r
binop Eq VNone VNone = return $ VBool True
binop Eq _ _ = return $ VBool False
binop Ne l r = binop Eq l r >>= \(VBool beq) -> pure . VBool $ not beq
binop Gt (VInt l) (VInt r) = return $ VBool $ l > r
binop Gt (VBool l) (VBool r) = return $ VBool $ l > r
binop Gt (VString l) (VString r) = return $ VBool $ l > r
binop Ge (VInt l) (VInt r) = return $ VBool $ l >= r
binop Ge (VBool l) (VBool r) = return $ VBool $ l >= r
binop Ge (VString l) (VString r) = return $ VBool $ l >= r
binop Lt (VInt l) (VInt r) = return $ VBool $ l < r
binop Lt (VBool l) (VBool r) = return $ VBool $ l < r
binop Lt (VString l) (VString r) = return $ VBool $ l < r
binop Le (VInt l) (VInt r) = return $ VBool $ l <= r
binop Le (VBool l) (VBool r) = return $ VBool $ l <= r
binop Le (VString l) (VString r) = return $ VBool $ l <= r
binop And (VBool l) (VBool r) = return $ VBool $ l && r
binop Or (VBool l) (VBool r) = return $ VBool $ l || r
binop op lhs rhs = failure $ BinTypeMismatch op lhs rhs

unop :: (Applicative m, Failable RuntimeError m) => Unop -> Value -> m Value
unop Not val = pure . VBool . not $ dummy val
unop Neg (VInt x) = pure . VInt $ negate x
unop op expr = failure $ UnTypeMismatch op expr


stringify :: Value -> String
stringify VNone = "None"
stringify (VBool x) = show x
stringify (VInt x) = show x
stringify (VString x) = x

builtin :: (Applicative t, Adaptable IO t, Failable RuntimeError t) => Builtin -> [Value] -> t Value
builtin Print msg = VNone <$ (adapt . putStrLn . concat $ stringify <$> msg)
builtin Input [] = adapt $ VString <$> getLine
builtin Str [v] = pure . VString $ stringify v
builtin Int [(VInt x)] = pure $ VInt x
builtin Int [(VBool False)] = pure $ VInt 0
builtin Int [(VBool True)] = pure $ VInt 1
builtin Int [(VString s)] = either (failure @RuntimeError . ValueError) (pure . VInt) $ readEither s
builtin Int [x] = failure . ValueError $ "Failed to parse int from " <> stringify x
builtin fun args = failure $ ArgumentMismatch (show fun) args


eval :: Expression -> Interpreter Value
eval (Literal x) = return x
eval (Variable ident) = getValue ident
eval (Binop op lhs rhs) = join $ binop op <$> eval lhs <*> eval rhs
eval (Unop op expr) = eval expr >>= unop op
eval (Call fun args) = getValue fun >>= \case
    VDef params block -> executeFunctionBlock fun args params block
    VBuiltin fun -> traverse eval args >>= builtin fun
    _ -> failure $ OutOfScope fun

early :: Interpreter Value -> Interpreter Value
early i = current @Callstack >>= adapt . result i >>= \case
    Left (EarlyExit v) -> pure v
    Left err -> failure err
    Right v -> pure v

result :: Interpreter a -> Callstack -> IO (Either RuntimeError a)
result i callstack = run $ snd <$> run i callstack

executeFunctionBlock :: Name -> [Expression] -> [Name] -> Block -> Interpreter Value
executeFunctionBlock fun args params block = do
    vals <- traverse eval args
    checkArity fun vals params
    modify (fromList (zip params vals) :) -- :)
    retval <- early $ interpret block $> VNone
    modify @Callstack tail
    pure retval

checkArity :: (Applicative m, Failable RuntimeError m) => Name -> [Value] -> [Name] -> m ()
checkArity fun vals params = when (length vals /= length params)
    . failure $ ArgumentMismatch fun vals

dummy :: Value -> Bool
dummy (VInt 0) = False
dummy (VString "") = False
dummy (VBool False) = False
dummy VNone = False
dummy _ = True

interpret :: Block -> Interpreter ()
interpret (Pure ()) = pure ()
interpret (Free (Instruct (Expression e) next)) = eval e *> interpret next
interpret (Free (Instruct (Define fun params block) next)) = setValue fun (VDef params block) *> interpret next
interpret (Free (Instruct (Assign lhs rhs) next)) = (eval rhs >>= setValue lhs) *> interpret next
interpret (Free (Instruct (If e true) next)) = (dummy <$> eval e >>= bool (pure ()) (interpret true)) *> interpret next
interpret this@(Free (Instruct (While e block) next)) = dummy <$> eval e >>= bool (interpret next) (interpret block *> interpret this)
interpret (Free (Instruct (Return e) next)) = eval e >>= failure @RuntimeError . EarlyExit


execute :: Interpreter () -> IO ()
execute i = result i [mempty] >>= \case
    Left (OutOfScope i) -> putStrLn $ "Name not found: " <> show i
    Left (BinTypeMismatch op lhs rhs) -> putStrLn $ "Attempt to perform " <> show op <> " on " <> show lhs <> " and " <> show rhs
    Left (UnTypeMismatch op expr) -> putStrLn $ "Attempt to perform " <> show op <> " on " <> show expr
    Left (ArgumentMismatch fun args) -> putStrLn $ "Attempt to call " <> fun <> " with " <> show (length args) <> " arguments"
    Left (EarlyExit _) -> putStrLn "Return from top level"
    Left (ValueError msg) -> putStrLn $ "ValueError: " <> msg
    Right () -> pure ()
