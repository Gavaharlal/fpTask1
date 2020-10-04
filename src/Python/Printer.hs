module Python.Printer where

import Python.AST
import Data.List
import Control.Monad.Free

indent :: Int -> ShowS
indent x = showString $ replicate (4 * x) ' '

binopPrec :: Binop -> Int
binopPrec Mul = 6
binopPrec Div = 6
binopPrec Mod = 6
binopPrec Add = 5
binopPrec Sub = 5
binopPrec And = 3
binopPrec Or = 2
binopPrec _ = 3

binopSymb :: Binop -> String
binopSymb Add = "+"
binopSymb Sub = "-"
binopSymb Mul = "*"
binopSymb Div = "/"
binopSymb Mod = "%"
binopSymb Eq  = "=="
binopSymb Ne  = "!="
binopSymb Gt  = ">"
binopSymb Ge  = ">="
binopSymb Lt  = "<"
binopSymb Le  = "<="
binopSymb And = "and"
binopSymb Or  = "or"

showsExpr :: Int -> Expression -> ShowS
showsExpr _ (Literal (VInt i)) = shows i
showsExpr _ (Literal (VBool b)) = shows b
showsExpr _ (Literal (VString s)) = shows s
showsExpr _ (Variable var) = showString var
showsExpr d (Binop op lhs rhs) = showParen (d > b) $
    showsExpr (b + 1) lhs . showString (" " <> binopSymb op <> " ") . showsExpr (b + 1) rhs
    where b = binopPrec op
showsExpr _ (Call fun args) = showString fun . (showParen True $
    foldr (.) id (intersperse (showString ", ") $ map (showsExpr 0) args))

pretty :: Int -> Block -> ShowS
pretty _ (Pure ()) = id
pretty i (Free (Instruct stmt next)) = indent i .f stmt . showChar '\n' . pretty i next
    where f (Expression expr) = showsExpr 0 expr
          f (Assign ident expr) = showString ident . showString " = " . showsExpr 0 expr
          f (Return expr) = showString "return " . showsExpr 0 expr
          f (If expr block) = showString "if " . showsExpr 0 expr . showString ":\n" . pretty (i + 1) block
          f (While expr block) = showString "while " . showsExpr 0 expr . showString ":\n" . pretty (i + 1) block
          f (Define name args block) = showString "def " . showString name . showString ("(" <> intercalate ", " args <> "):\n") . pretty (i + 1) block

prettyPrint :: Block -> String
prettyPrint block = pretty 0 block ""
