import Test.Hspec (describe, hspec, it, shouldBe)
import Text.Megaparsec (parse)

import Python.AST
import Python.Parser

exampleSource = unlines
    [ "x = 5"
    , "y = 6"
    , "temp = x"
    , "x = y"
    , "y = temp"
    , "xy = x + y"
    , "print(xy)"
    , "def sumnums (f, s):"
    , "   r = f + s"
    , "   return r"
    ]

exampleDSL = do
    command . Assign "x" . Literal $ VInt 5
    command . Assign "y" . Literal $ VInt 6
    command . Assign "temp" $ Variable "x"
    command . Assign "x" $ Variable "y"
    command . Assign "y" $ Variable "temp"
    command . Assign "xy" $ Binop Add (Variable "x") (Variable "y")
    command . Expression $ Call "print" [Variable "xy"]
    command . Define "sumnums" ["f", "s"] $ do
        command . Assign "r" $ Binop Add (Variable "f") (Variable "s")
        command . Return $ Variable "r"

main :: IO ()
main = hspec . describe "parser" . it "parsing source code" $
    parse python "" exampleSource `shouldBe` Right exampleDSL
