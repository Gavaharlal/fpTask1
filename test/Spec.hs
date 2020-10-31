import Test.Hspec (describe, hspec, it, shouldBe)
import Text.Megaparsec (parse)

import Control.Monad.State.Strict
import Control.Lens ((^.))

import Python.AST
import Python.Parser
import Python.Interpreter (TestEnv, interpret, PureIO(..), execute, input, output)
import Utils (parsing)
import Python.Printer (prettyprintSource, prettyprintEDSL)
import Data.Either (isLeft)

exampleStringDSLPrettyPrint = unlines [ "do",
                               "    assign \"x\" $ Literal (VInt 5)",
                               "    expr $ Call \"print\" [Variable \"a\",Variable \"b\",Variable \"d\"]",
                               "    assign \"x\" $ Binop Or (Literal (VBool True)) (Literal (VBool False))",
                               "    assign \"a\" $ Call \"input\" []",
                               "    expr $ Call \"print\" [Binop Sub (Binop Add (Literal (VInt 3)) (Literal (VInt 4))) (Binop Mul (Literal (VInt 8)) (Literal (VInt 9)))]"
                              ]

exampleDSLP1 = block [ Assign "x" . Literal $ VInt 5
                     , Assign "y" . Literal $ VInt 6
                     , Assign "temp" $ Binop Sub (Binop Add (Variable "x") (Variable "x")) (Binop Mul (Variable "y") (Literal (VInt 5)))
                     , Assign "x" $ Variable "y"
                     , Assign "y" $ Variable "temp"
                     , Assign "xy" $ Binop Sub (Variable "x") (Variable "y")
                     , Expression $ Call "print" [Variable "xy"]
                     , Define "sumnums" ["f", "s"] $ block
                        [ Assign "r" $ Binop Add (Variable "f") (Variable "s")
                        , Return $ Variable "r"
                        ]
                     ]
                     
exampleDSLP2 = do 
    command . Define "func" ["a", "b"] $ do
        command . Return $ Binop Add (Variable "a") (Variable "b")
        


prettyPrintedCode = unlines [ "x = 5",
                              "print(a, b, d)",
                              "x = True or False",
                              "a = input()",
                              "print((3 + 4) - 8 * 9)"
                             ]


testEnvDSL ::Block -> TestEnv ()
testEnvDSL dsl = execute $ interpret dsl

myGCD :: Integer -> Integer -> Integer
myGCD a b
      | b == 0     = abs a
      | otherwise  = myGCD b (a `mod` b)
      
checkException :: String -> FilePath -> IO ()
checkException excMessage fileName = do
    prog_DSL <- parsing fileName
    let env = execState (testEnvDSL prog_DSL) PureIO { _input = [], _output = [] }
    
    null (env ^. input) `shouldBe` True
    length (env ^. output) `shouldBe` 1
    head (env ^. output) `shouldBe` excMessage       


main :: IO ()
main = hspec $ do
    describe "Test interpreter evaluation" $ do
        sequence_ $ flip map [500..600] $ \a ->
            sequence_ $ flip map [500..600] $ \b -> 
                    it ("check GCD for " <> show a <> " and " <> show b) $ do
                        let fileName = "testSources/correct/gcd.py"
                        pyGCD_DSL <- parsing fileName
                        let env = execState (testEnvDSL pyGCD_DSL) PureIO { _input = [show a, show b], _output = [] }
                        
                        null (env ^. input) `shouldBe` True
                        length (env ^. output) `shouldBe` 1
                        read (head (env ^. output)) `shouldBe` myGCD a b
                        
    describe "Test interpreter exceptions" $ do
        it "check ValueError" $ do
            let excMessage = "Error converting string \"str\" to int"
            let fileName = "testSources/incorrect/errValueError.py"
            checkException excMessage fileName

        it "check OutOfScope" $ do
            let excMessage = "Name not found: \"a\""
            let fileName = "testSources/incorrect/errOutOfScope.py"
            checkException excMessage fileName
            
        it "check ArgumentMismatch" $ do
            let excMessage = "Attempt to call funct with 3 arguments"
            let fileName = "testSources/incorrect/errArgumentMismatch.py"
            checkException excMessage fileName
            
        it "check BinTypeMismatch" $ do
            let excMessage = "Attempt to perform Div on VInt 100 and VString \"str\""
            let fileName = "testSources/incorrect/errBinTypeMismatch.py"
            checkException excMessage fileName
    
        it "check UnTypeMismatch" $ do
            let excMessage = "Attempt to perform Neg on VString \"str\""
            let fileName = "testSources/incorrect/errUnTypeMismatch.py"
            checkException excMessage fileName     
            
        it "check EarlyExit" $ do
            let excMessage = "Return from top level"
            let fileName = "testSources/incorrect/errEarlyExit.py"
            checkException excMessage fileName            
                        
                        
    describe "Test parser" $ do
        it "should parse program (\"testSources/correct/p1.py\")" $ do
            let fileName = "testSources/correct/p1.py"
            dslBlock <- parsing fileName
            dslBlock `shouldBe` exampleDSLP1
    
        it "should parse valid ident (\"testSources/correct/p2.py\")" $ do
            let fileName = "testSources/correct/p2.py"
            dslBlock <- parsing fileName
            dslBlock `shouldBe` exampleDSLP2
    
        it "should fail invalid expression (\"testSources/incorrect/f1.py\")" $ do
            let fileName = "testSources/incorrect/f1.py"
            pCode <- readFile fileName
            let ei = parse python pCode ""
            isLeft ei `shouldBe` True
    
        it "should fail invalid indent (\"testSources/incorrect/f2.py\")" $ do
            let fileName = "testSources/incorrect/f2.py"
            pCode <- readFile fileName
            let ei = parse python pCode ""
            isLeft ei `shouldBe` True
    
    
    describe "Test pretty printing" $ do
        it "prettyPrinting source code (\"testSources/correct/prettyPrint.py\")" $ do
            let fileName = "testSources/correct/prettyPrint.py"
            dslBlock <- parsing fileName
            prettyprintSource dslBlock `shouldBe`  prettyPrintedCode
        it "prettyPrinting edsl (\"testSources/correct/prettyPrint.py\")" $ do
            let fileName = "testSources/correct/prettyPrint.py"
            dslBlock <- parsing fileName
            prettyprintEDSL dslBlock `shouldBe` exampleStringDSLPrettyPrint            
        