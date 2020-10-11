module Python.Parser where

import Data.Functor (($>))
import Data.Void (Void)
import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, space1, string, tab)
import Text.Megaparsec.Char.Lexer
import Control.Monad.Combinators.Expr

import Python.AST

type Parser = Parsec Void String

python :: Parser Block
python = parseTopLevelBlock <* eof where
    parseTopLevelBlock :: Parser Block
    parseTopLevelBlock = nonIndented scn
        $ block <$> some parseStatement

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

parseTerm :: Parser Expression
parseTerm = parens parseExpression
    <|> try parseCall
    <|> Variable <$> try parseName
    <|> Literal . VInt <$> parseInt
    <|> Literal . VString <$> parseStr
    <|> Literal . VBool <$> parseBool
    <|> reserved "None" $> Literal VNone

parseCall :: Parser Expression
parseCall = Call <$> parseName <*> parens (sepBy parseExpression $ symbol sc ",")


parseExpression :: Parser Expression
parseExpression = makeExprParser parseTerm $
    [ [ Prefix (Unop Neg <$ symbol sc "-") ]
    , [ Prefix (Unop Not <$ symbol sc "not") ]
    , [ InfixL (Binop Mul <$ symbol sc "*")
    , InfixL (Binop Div <$ symbol sc "/")
    , InfixL (Binop Mod <$ symbol sc "%") ]
    , [ InfixL (Binop Add <$ symbol sc "+")
    , InfixL (Binop Sub <$ symbol sc "-") ]
    , [ InfixL (Binop Eq <$ symbol sc "==")
    , InfixL (Binop Ne <$ symbol sc "!=")
    , InfixL (Binop Ge <$ symbol sc ">=")
    , InfixL (Binop Le <$ symbol sc "<=")
    , InfixL (Binop Gt <$ symbol sc ">")
    , InfixL (Binop Lt <$ symbol sc "<") ]
    , [ InfixL (Binop And <$ symbol sc "and") ]
    , [ InfixL (Binop Or <$ symbol sc "or") ]
    ]
    
parseExpressionStatement :: Parser Statement
parseExpressionStatement = Expression <$> parseExpression <* scn

parseAssign :: Parser Statement
parseAssign = Assign <$> parseName <* symbol sc "=" <*> parseExpression <* scn

parseReturn :: Parser Statement
parseReturn = reserved "return" *> (Return <$> parseExpression) <* scn

parseIf :: Parser Statement
parseIf = indented $ parseSubBlock . If <$>
    between (reserved "if") (symbol sc ":") parseExpression

parseWhile :: Parser Statement
parseWhile = indented $ parseSubBlock . While <$>
    (reserved "while" *> parseExpression <* symbol sc ":")    

parseStatement :: Parser Statement
parseStatement = choice $ try <$> [parseAssign, parseExpressionStatement, parseReturn, parseIf, parseWhile, parseDef]