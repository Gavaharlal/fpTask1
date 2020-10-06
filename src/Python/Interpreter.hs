module Python.Interpreter where

import Python.AST
import Control.Monad
import Control.Monad.Free
import Control.Joint
import Control.Lens (element, (%~))
import Control.Monad.Except
import Data.Functor
import qualified Data.Map.Strict as M
import Text.Read (readEither)

type Scope = M.Map Name Value

data RuntimeError = OutOfScope Name
                  | BinTypeMismatch Binop Value Value
                  | ArgumentMismatch Name Int
                  | EarlyExit Value
                  | ValueError String
                  deriving (Eq, Show)

type Interpreter = State [Scope] :> Either RuntimeError :> IO

getValue :: Name -> Interpreter Value
getValue "print" = return $ VBuiltin Print
getValue "input" = return $ VBuiltin Input
getValue "str" = return $ VBuiltin Str
getValue "int" = return $ VBuiltin Int
getValue _ = error "Not yet implemented" -- TODO var matching

setValue :: Name -> Value -> Interpreter ()
setValue ident value = modify @[Scope] $ element 0 %~ M.insert ident value

binop :: Binop -> Value -> Value -> Interpreter Value
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
binop Ne l r = binop Eq l r >>= \case
    VBool beq -> pure . VBool $ not beq
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


stringify :: Value -> String
stringify VNone = "None"
stringify (VBool x) = show x
stringify (VInt x) = show x
stringify (VString x) = x

builtin :: Builtin -> [Value] -> Interpreter Value
builtin Print msg = do
    let s = concat $ map stringify msg
    lift $ lift $ putStr s
    return VNone
builtin Input [] = do
    s <- lift $ lift getLine
    return $ VString s
builtin Input args = failure $ ArgumentMismatch "input" (length args)
builtin Str [v] = return . VString $ stringify v
builtin Int [(VInt x)] = return $ VInt x
builtin Int [(VBool False)] = return $ VInt 0
builtin Int [(VBool True)] = return $ VInt 1
builtin Int [(VString s)] = case readEither s of
                              Left err -> failure $ ValueError err
                              Right val -> return $ VInt val
builtin Int [x] = failure . ValueError $ "Failed to parse int from " <> (stringify x)
builtin fun args = failure $ ArgumentMismatch (show fun) (length args)



eval :: Expression -> Interpreter Value
eval (Literal x) = return x
eval (Variable ident) = getValue ident
eval (Binop op lhs rhs) = do
    l <- eval lhs
    r <- eval rhs
    binop op l r
eval (Call fun args) = do
    f <- getValue fun
    vals <- mapM eval args
    case f of
      VDef params block -> do
          when (length vals /= length params) $
              failure $ ArgumentMismatch fun (length vals)
          modify (M.fromList (zip params vals):)
          retval <- interpret block $> VNone --dummy for now TODO early return
          modify @[Scope] tail -- pop frame
          return retval
      VBuiltin fun -> builtin fun vals
      _ -> failure $ OutOfScope fun

evalBool :: Expression -> Interpreter Bool
evalBool e = do
    val <- eval e
    return $ case val of
      VInt 0 -> False
      VString "" -> False
      VBool False -> False
      VNone -> False
      _ -> True

interpret :: Block -> Interpreter ()
interpret (Pure ()) = return ()
interpret this@(Free (Instruct stmt next)) = do
    case stmt of
      Expression e -> do
          eval e
          interpret next
      Assign lhs rhs -> do
          v <- eval rhs
          setValue lhs v
          interpret next
      Return e -> do
          eval e
          return ()
      If e true -> do
          val <- evalBool e
          when val $ interpret true >> return ()
          interpret next
      While e block -> do
          val <- evalBool e
          if val then interpret block >> interpret this
                 else interpret next
      Define fun params block -> do
          setValue fun $ VDef params block
          interpret next

execute :: Interpreter () -> IO ()
execute i = run (snd <$> run i [mempty]) >>= \case
    Left (OutOfScope i) -> putStrLn $ "Identifier not found: " <> show i
    Left (BinTypeMismatch op lhs rhs) -> putStrLn $ "Attempt to perform " <> show op <> " on " <> show lhs <> " and " <> show rhs
    Left (ArgumentMismatch fun n) -> putStrLn $ "Attempt to call " <> fun <> " with " <> show n <> " arguments"
    Right () -> pure ()
