module Python.Interpreter where

import Python.AST
import Prelude hiding (lookup)
import Control.Monad
import Control.Monad.Free
import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Lens (makeLenses, element, (%~), (^.), (<>~), (.=))
import Data.Bool
import Data.Functor
import Data.Map.Strict (Map, fromList, insert, lookup)
import Text.Read (readMaybe)
import Data.List (intercalate)

type Scope = Map Name Value
type Callstack = [Scope]

data RuntimeError =  ValueError String
                  | OutOfScope Name
                  | ArgumentMismatch Name [Value]
                  | BinTypeMismatch Binop Value Value
                  | UnTypeMismatch Unop Value
                  | EarlyExit Value
                  deriving (Show)

data PureIO = PureIO {_input :: [String], _output :: [String] }
type TestEnv = State PureIO

makeLenses ''PureIO

class Monad m => Interpretable m where
   writeToEnv :: String -> m ()
   readFromEnv :: m String

instance Interpretable IO where
    writeToEnv = putStrLn
    readFromEnv = getLine

instance Interpretable TestEnv where
    writeToEnv s = modify $ output <>~ [s]
    readFromEnv = do
        st <- get
        let curInput = st ^. input
        when (null curInput) $ error "Insufficient input. At least one more argument was expected."
        let r = head curInput
        modify $ input %~ tail
        return r

type InterpreterT m = StateT Callstack (ExceptT RuntimeError m)

getValue :: (Interpretable m) => Name -> InterpreterT m Value
getValue "print" = return $ VBuiltin Print
getValue "input" = return $ VBuiltin Input
getValue "str" = return $ VBuiltin Str
getValue "int" = return $ VBuiltin Int
getValue ident = do
    callStack <- get
    let value = msum $ lookup ident <$> callStack
    maybe (throwError $ OutOfScope ident) return value

setValue :: (Interpretable m) => Name -> Value -> InterpreterT m ()
setValue ident value = modify @Callstack $ element 0 %~ insert ident value

binop :: MonadError RuntimeError m => Binop -> Value -> Value -> m Value
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
binop op lhs rhs = throwError $ BinTypeMismatch op lhs rhs

unop :: MonadError RuntimeError f => Unop -> Value -> f Value
unop Not val = pure . VBool . not $ dummy val
unop Neg (VInt x) = pure . VInt $ negate x
unop op expr = throwError $ UnTypeMismatch op expr


stringify :: Value -> String
stringify VNone = "None"
stringify (VBool x) = show x
stringify (VInt x) = show x
stringify (VString x) = x
stringify (VDef args _) = "f(" <> intercalate ", " args <> ")"
stringify (VBuiltin x) = show x

builtin :: (Interpretable m) => Builtin -> [Value] -> InterpreterT m Value
builtin Print msg = VNone <$ lift (lift $ writeToEnv . concat $ stringify <$> msg)
builtin Input [] = VString <$> lift (lift readFromEnv)
builtin Str [v] = pure . VString $ stringify v
builtin Int [VInt x] = pure $ VInt x
builtin Int [VBool False] = pure $ VInt 0
builtin Int [VBool True] = pure $ VInt 1
builtin Int [VString s] = maybe (throwError $ ValueError errMsg) (pure . VInt) $ readMaybe s
    where errMsg = "Error converting string \"" <> s <> "\" to int"
builtin Int [x] = throwError . ValueError $ "Failed to parse int from " <> stringify x
builtin fun args = throwError $ ArgumentMismatch (show fun) args


eval :: Interpretable m => Expression -> InterpreterT m Value
eval (Literal x) = return x
eval (Variable ident) = getValue ident
eval (Binop op lhs rhs) = join $ binop op <$> eval lhs <*> eval rhs
eval (Unop op expr) = eval expr >>= unop op
eval (Call fun args) = getValue fun >>= \case
    VDef params block -> executeFunctionBlock fun args params block
    VBuiltin fun -> mapM eval args >>= builtin fun
    _ -> throwError $ OutOfScope fun


executeFunctionBlock :: Interpretable m => Name
                        -> [Expression]
                        -> [Name]
                        -> Block
                        -> InterpreterT m Value
executeFunctionBlock fun args params bl = do
    vals <- traverse eval args
    checkArity fun vals params
    modify (fromList (zip params vals) :) -- :)
    retval <- early $ interpret bl $> VNone
    modify @Callstack tail
    pure retval


early :: (Interpretable m) => InterpreterT m Value -> InterpreterT m Value
early i = get @Callstack >>= lift . lift . result i >>= \case
    Left (EarlyExit v) -> pure v
    Left err -> throwError err
    Right v -> pure v

result :: Interpretable m => StateT Callstack (ExceptT RuntimeError m) a
          -> Callstack -> m (Either RuntimeError a)
result i callstack = runExceptT $ evalStateT i callstack

checkArity :: (Foldable t, MonadError RuntimeError f) => Name -> [Value] -> t a -> f ()
checkArity fun vals params = when (length vals /= length params)
    . throwError $ ArgumentMismatch fun vals

dummy :: Value -> Bool
dummy (VInt 0) = False
dummy (VString "") = False
dummy (VBool False) = False
dummy VNone = False
dummy _ = True

interpret :: (Interpretable m) => Block -> InterpreterT m ()
interpret (Pure ()) = pure ()
interpret (Free (Instruct (Expression e) next)) = eval e *> interpret next
interpret (Free (Instruct (Define fun params block) next)) = setValue fun (VDef params block) *> interpret next
interpret (Free (Instruct (Assign lhs rhs) next)) = (eval rhs >>= setValue lhs) *> interpret next
interpret (Free (Instruct (If e true) next)) = (dummy <$> eval e >>= bool (pure ()) (interpret true)) *> interpret next
interpret this@(Free (Instruct (While e block) next)) = dummy <$> eval e >>= bool (interpret next) (interpret block *> interpret this)
interpret (Free (Instruct (Return e) next)) = eval e >>= throwError @RuntimeError . EarlyExit


execute :: (Interpretable m) => InterpreterT m () -> m ()
execute i = result i [mempty] >>= \case
    Left (OutOfScope i) -> writeToEnv $ "Name not found: " <> show i
    Left (BinTypeMismatch op lhs rhs) -> writeToEnv $ "Attempt to perform " <> show op <> " on " <> show lhs <> " and " <> show rhs
    Left (UnTypeMismatch op expr) -> writeToEnv $ "Attempt to perform " <> show op <> " on " <> show expr
    Left (ArgumentMismatch fun args) -> writeToEnv $ "Attempt to call " <> fun <> " with " <> show (length args) <> " arguments"
    Left (EarlyExit _) -> writeToEnv "Return from top level"
    Left (ValueError s) -> writeToEnv $ s
    Right () -> pure ()
