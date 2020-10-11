module Python.Parser where

import Data.Functor (($>))
import Data.Void (Void)
import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, space1, string, tab)
import Text.Megaparsec.Char.Lexer

import Python.AST

type Parser = Parsec Void String

comment :: Parser ()
comment = skipLineComment "#"

sc :: Parser ()
sc = space (void $ char ' ' <|> tab) comment empty

scn :: Parser ()
scn = space space1 comment empty

parens = between (symbol sc "(") (symbol sc ")")

reserved :: Name -> Parser ()
reserved w = between (string w) sc $ notFollowedBy alphaNumChar

indented :: Parser (IndentOpt Parser a b) -> Parser a
indented = indentBlock scn

parseInt :: Parser Int
parseInt = lexeme sc $ choice
    [decimal, string "0b" *> binary, string "0o" *> octal, string "0x" *> hexadecimal]

parseBool :: Parser Bool
parseBool = reserved "True" $> True <|> reserved "False" $> False

parseStr :: Parser String
parseStr = (char '"' *> manyTill charLiteral (char '"'))
    <|> (char '\'' *> manyTill charLiteral (char '\''))

parseName :: Parser Name
parseName = lexeme sc . try $ identifier >>= notReserved where
    identifier :: Parser Name
    identifier = (:) <$> letterChar <*> many alphaNumChar
    reserved :: [Name]
    reserved = ["def", "if", "while", "return", "not", "and", "or", "True", "False", "None"]
    notReserved :: Name -> Parser Name
    notReserved x = if elem x reserved
        then fail $ "Fail: " <> show x
        else return x

parseDef :: Parser Statement
parseDef = indented $ reserved "def" *> (parseSubBlock <$> define) where

    define :: Parser (Block -> Statement)
    define = Define <$> parseName <*> arguments

    arguments :: Parser [Name]
    arguments = parens (sepBy parseName $ symbol sc ",") <* symbol sc ":"

parseSubBlock :: (Block -> Statement) -> IndentOpt Parser Statement Statement
parseSubBlock f = IndentSome Nothing (pure . f . block) parseStatement

parseStatement :: Parser Statement
parseStatement = error "todo"