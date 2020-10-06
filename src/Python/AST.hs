module Python.AST where

import Data.Functor.Classes
import Data.List
import Control.Monad.Free

type Name = String

newtype Sourcecode a = Sourcecode a

class Precedence a where
    precedence :: a -> Int

data Value = VInt Int
           | VBool Bool
           | VString String
           | VDef [Name] Block
           | VBuiltin Builtin
           | VNone
           deriving (Eq, Show)

data Unop = Not | Neg
    deriving (Eq, Show, Enum)

instance Precedence Unop where
    precedence Not = 7
    precedence Neg = 8

instance Show (Sourcecode Unop) where
    show (Sourcecode Not) = "not "
    show (Sourcecode Neg) = "-"

data Binop = Add | Sub | Mul | Div | Mod | Eq | Ne | Gt | Ge | Lt | Le | And | Or
    deriving (Eq, Show, Enum)

instance Precedence Binop where
    precedence Mul = 6
    precedence Div = 6
    precedence Mod = 6
    precedence Add = 5
    precedence Sub = 5
    precedence And = 3
    precedence Or  = 2
    precedence _   = 4

instance Show (Sourcecode Binop) where
    show (Sourcecode Add) = "+"
    show (Sourcecode Sub) = "-"
    show (Sourcecode Mul) = "*"
    show (Sourcecode Div) = "/"
    show (Sourcecode Mod) = "%"
    show (Sourcecode Eq)  = "=="
    show (Sourcecode Ne)  = "!="
    show (Sourcecode Gt)  = ">"
    show (Sourcecode Ge)  = ">="
    show (Sourcecode Lt)  = "<"
    show (Sourcecode Le)  = "<="
    show (Sourcecode And) = "and"
    show (Sourcecode Or)  = "or"

binopPrec :: Binop -> Int
binopPrec Mul = 6
binopPrec Div = 6
binopPrec Mod = 6
binopPrec Add = 5
binopPrec Sub = 5
binopPrec And = 3
binopPrec Or  = 2
binopPrec _   = 4

data Builtin = Print | Input | Str | Int
    deriving (Eq, Show)

data Expression
    = Literal Value
    | Variable Name
    | Binop Binop Expression Expression
    | Unop Unop Expression
    | Call Name [Expression]
    deriving (Eq, Show)

instance Show (Sourcecode Expression) where
    showsPrec _ (Sourcecode (Literal (VInt i))) = shows i
    showsPrec _ (Sourcecode (Literal (VBool b))) = shows b
    showsPrec _ (Sourcecode (Literal (VString s))) = shows s
    showsPrec _ (Sourcecode (Literal VNone)) = showString "None"
    showsPrec _ (Sourcecode (Variable var)) = showString var
    showsPrec d (Sourcecode (Binop op lhs rhs)) = showParen (d > precedence op) $ showsPrec (precedence op + 1) (Sourcecode lhs)
        . showString (" " <> show (Sourcecode op) <> " ") . showsPrec (precedence op + 1) (Sourcecode rhs)
    showsPrec d (Sourcecode (Unop op expr)) = showParen (d > precedence op)
        $ showString (show $ Sourcecode op) . showsPrec (precedence op + 1) (Sourcecode expr)
    showsPrec _ (Sourcecode (Call fun args)) = showString fun . (showParen True $
        foldr (.) id (intersperse (showString ", ") $ showsPrec 0 . Sourcecode <$> args))

data Statement = Expression Expression
               | Define Name [Name] Block
               | Assign Name Expression
               | If Expression Block
               | While Expression Block
               | Return Expression
               deriving (Eq, Show)

data Instruct next = Instruct Statement next
    deriving (Show, Show1, Functor)

instance Eq1 Instruct where
    liftEq eq (Instruct stmt next) (Instruct stmt' next') = stmt == stmt' && eq next next'

type Block = Free Instruct ()

block :: [Statement] -> Block
block stmts = sequence_ $ liftF . flip Instruct () <$> stmts

command :: Statement -> Free Instruct ()
command stmt = liftF $ Instruct stmt ()

example :: Block
example = block
        [Assign "a" (Literal $ VInt 1),
         Assign "b" (Literal $ VInt 10),
         While (Binop Lt (Variable "a") (Variable "b"))
             (block [Assign "a" (Binop Add (Variable "a") (Variable "a"))])]

