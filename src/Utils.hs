module Utils where

import Python.AST (Block)
import Python.Parser (python)
import Text.Megaparsec (errorBundlePretty, parse)

parsing :: FilePath -> IO Block
parsing f =
    readFile f
        >>= either (error . errorBundlePretty) pure . parse python f
