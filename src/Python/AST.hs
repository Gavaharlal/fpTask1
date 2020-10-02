module Python.AST where

import Control.Applicative
import Control.Monad
import Control.Monad.Free
import Data.Functor.Classes

type Name = String

data Value = VInt Int
           | VBool Bool
           | VString String
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


expr :: Expression -> Free Instruct ()
expr = liftF . flip Instruct () . Expression

assign :: Name -> Expression -> Free Instruct ()
assign lhs rhs = liftF . flip Instruct () $ Assign lhs rhs

ret :: Expression -> Free Instruct ()
ret e = liftF . flip Instruct () $ Return e

if' :: Expression -> Free Instruct () -> Free Instruct ()
if' e true = liftF . flip Instruct () $ If e true

while :: Expression -> Block -> Free Instruct ()
while e block = liftF . flip Instruct () $ While e block

def :: Name -> [Name] -> Block -> Free Instruct ()
def name args block = liftF . flip Instruct () $ Define name args block

type Block = Free Instruct ()

example :: Block
example = do
    assign "a" (Literal $ VInt 1)
    assign "b" (Literal $ VInt 10)
    while (Binop Lt (Variable "a") (Variable "b")) $ do
        assign "a" (Binop Add (Variable "a") (Variable "a"))

