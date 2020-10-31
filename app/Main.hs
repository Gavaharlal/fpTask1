module Main where

import Python.Interpreter (execute, interpret)
import Python.Printer (prettyprintEDSL, prettyprintSource)
import System.Environment (getArgs, getProgName)
import Utils (parsing)


proceed :: [String] -> IO ()
proceed ["prettify", f] = putStr . prettyprintSource =<< parsing f
proceed ["edsl", f] = putStr . prettyprintEDSL =<< parsing f
proceed ["execute", f] = execute . interpret =<< parsing f
proceed _ = getProgName >>= putStrLn . tip
  where
    tip name =
      concatMap
        ( \d ->
            "\t\t" <> name <> " "
              <> fst d
              <> " <filepath> "
              <> snd d
              <> " specified source code file\n"
        )
        instructions
    instructions =
      [ ("prettify", "for pretty-printing"),
        ("edsl", "for printing edsl of the"),
        ("execute", "to execute")
      ]

main :: IO ()
main = getArgs >>= proceed
