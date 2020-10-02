module Python.Printer where

import Python.AST
import Data.List
import Control.Monad.Free

indent :: Int -> ShowS
indent x = showString $ replicate (4 * x) ' '

pretty :: Int -> Block -> ShowS
pretty _ (Pure ()) = id
pretty i (Free (Instruct stmt next)) = indent i .f stmt . showChar '\n' . pretty i next
    where f (Expression expr) = shows expr
          f (Assign ident expr) = showString ident . showString " = " . shows expr
          f (Return expr) = showString "return " . shows expr
          f (If expr block) = showString "if " . shows expr . showString ":\n" . pretty (i + 1) block
          f (While expr block) = showString "while " . shows expr . showString ":\n" . pretty (i + 1) block
          f (Define name args block) = showString "def " . showString name . showString ("(" <> intercalate ", " args <> "):\n") . pretty (i + 1) block
