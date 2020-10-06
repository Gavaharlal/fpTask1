module Python.AST where

import Control.Applicative
import Control.Monad
import Control.Monad.Free
import Data.Functor.Classes

type Name = String

data Builtin = Print | Input | Str | Int
    deriving (Eq, Show)

data Value = VInt Int
           | VBool Bool
           | VString String
           | VDef [Name] Block
           | VBuiltin Builtin
           | VNone
           deriving (Eq, Show)

data Binop = Add
           | Sub
           | Mul
           | Div
           | Mod
           | Eq
           | Ne
           | Gt
           | Ge
           | Lt
           | Le
           | And
           | Or
           deriving (Eq, Show, Enum)

data Unop = Not | Neg
    deriving (Eq, Show, Enum)

data Expression = Literal Value
          | Variable Name
          | Binop Binop Expression Expression
          | Unop Unop Expression
          | Call Name [Expression]
          deriving (Eq, Show)

data Statement = Expression Expression
               | Assign Name Expression
               | Return Expression
               | If Expression Block
               | While Expression Block
               | Define Name [Name] Block
               deriving (Eq, Show)

data Instruct next = Instruct Statement next
    deriving (Show, Show1, Functor)

instance Eq1 Instruct where
    liftEq eq (Instruct stmt next) (Instruct stmt' next') = stmt == stmt' && eq next next'

command :: Statement -> Free Instruct ()
command stmt = liftF $ Instruct stmt ()

type Block = Free Instruct ()

block :: [Statement] -> Block
block stmts = sequence_ $ liftF . flip Instruct () <$> stmts

--example :: Block
--example = do
--    assign "a" (Literal $ VInt 1)
--    assign "b" (Literal $ VInt 10)
--    while (Binop Lt (Variable "a") (Variable "b")) $ do
--        assign "a" (Binop Add (Variable "a") (Variable "a"))
--
