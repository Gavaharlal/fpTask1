import Test.Hspec (describe, hspec, it, shouldBe)
import Text.Megaparsec (parse)

import Control.Monad.State.Strict
import Control.Lens ((^.))

import Python.AST
import Python.Parser
import Python.Interpreter (TestEnv, interpret, PureIO(..), execute, input, output)
import Utils (parsing)

exampleDSL = do
    command . Assign "x" . Literal $ VInt 5
    command . Assign "y" . Literal $ VInt 6
    command . Assign "temp" $ Variable "x"
    command . Assign "x" $ Variable "y"
    command . Assign "y" $ Variable "temp"
    command . Assign "xy" $ Binop Sub (Variable "x") (Variable "y")
    command . Expression $ Call "print" [Variable "xy"]
    command . Define "sumnums" ["f", "s"] $ do
        command . Assign "r" $ Binop Add (Variable "f") (Variable "s")
        command . Return $ Variable "r"


testEnvDSL ::Block -> TestEnv ()
testEnvDSL dsl = execute $ interpret dsl

main :: IO ()
main = do
    p1 <- readFile "testSources/p1.py"

    dsl <- parsing "testSources/ex2.py"

    let env = execState (testEnvDSL dsl) PureIO { _input = ["12", "2", "7", "282"], _output = [] }

    putStrLn "before input"
    putStrLn . unlines $ env ^. input
    putStrLn "after input"
    putStrLn . unlines $ env ^. output

    hspec $ do 
        describe "parser" $ do
            it "parsing source code" $
                parse python "" p1 `shouldBe` Right exampleDSL

        describe "Interpreting" $ do
            it "Hello World" $ do
                "xdsf" `shouldBe` "xdsf"
