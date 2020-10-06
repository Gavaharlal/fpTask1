module Python.Printer where

import Data.List
import Control.Monad.Free
import Python.AST

indent :: Int -> ShowS
indent x = showString $ replicate (4 * x) ' '

prettyprintSource :: Block -> String
prettyprintSource block = prettyInstruction 0 block "" where
    prettyInstruction _ (Pure ()) = id
    prettyInstruction i (Free (Instruct stmt next)) = indent i . f stmt . showChar '\n' . prettyInstruction i next where
        f (Expression expr) = showsPrec 0 (Sourcecode expr)
        f (Assign ident expr) = showString ident . showString " = " . showsPrec 0 (Sourcecode expr)
        f (Return expr) = showString "return " . showsPrec 0 (Sourcecode expr)
        f (If expr block) = showString "if " . showsPrec 0 (Sourcecode expr) . showString ":\n" . prettyInstruction (i + 1) block
        f (While expr block) = showString "while " . showsPrec 0 (Sourcecode expr) . showString ":\n" . prettyInstruction (i + 1) block
        f (Define name args block) = showString "def " . showString name . showString ("(" <> intercalate ", " args <> "):\n") . prettyInstruction (i + 1) block

prettyprintEDSL :: Block -> String
prettyprintEDSL block = "do\n" <> prettyInstruction 1 block "" where
    prettyInstruction _ (Pure ()) = id
    prettyInstruction i (Free (Instruct stmt next)) = indent i . f stmt . showChar '\n' . prettyInstruction i next where
        f (Expression expr) = showString "expr $ " . shows expr
        f (Assign ident expr) = showString "assign " . shows ident . showString " $ " . shows expr
        f (Return expr) = showString "ret $ " . shows expr
        f (If expr block) = showString "if' (" . shows expr . showString ") $ do\n" . prettyInstruction (i + 1) block
        f (While expr block) = showString "while (" . shows expr . showString ") $ do\n" . prettyInstruction (i + 1) block
        f (Define name args block) = showString "def " . shows name . showChar ' ' . shows args . showString " $ do\n" . prettyInstruction (i + 1) block
