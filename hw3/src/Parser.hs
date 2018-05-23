module Parser where

import Expr (Expr (..), Stmt (..))

import Control.Applicative (empty)
import Control.Monad.Catch (Exception, MonadThrow, throwM)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Expr

type Parser = Parsec Void String

space1 :: Parser ()
space1 = skipSome (char ' ')

sc :: Parser () -- space consumer
sc = L.space Parser.space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser Int
integer = lexeme L.decimal

rword :: String -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

rws :: [String] -- list of reserved words
rws = ["mut", "let"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

stmt :: Parser Stmt
stmt = def <|> wrt <|> rd <|> upd

def :: Parser Stmt
def = Def <$> (rword "mut" *> identifier <* symbol "=") <*> expr

upd :: Parser Stmt
upd = Upd <$> (identifier <* symbol "=") <*> expr

wrt :: Parser Stmt
wrt = Wrt <$> (symbol "<" *> expr)

rd :: Parser Stmt
rd = Rd <$> (symbol ">" *> identifier)

stmts :: Parser [Stmt]
stmts = sc *> many (stmt <* eol)

term :: Parser Expr
term = lett <|> var <|> con <|> parens expr

lett :: Parser Expr
lett = Let <$> (symbol "(" *> rword "let" *> identifier <* symbol "=")
            <*> expr <*> (symbol "in" *> expr <* symbol ")")

var :: Parser Expr
var = Var <$> identifier

con :: Parser Expr
con = Const <$> integer

operators :: [[Operator Parser Expr]]
operators =
    [ [
    InfixL (Mul <$ symbol "*")
    , InfixL (Div <$ symbol "/")
    ]
    , [
    InfixL (Add <$ symbol "+")
    , InfixL (Sub <$ symbol "-")
    ]
    ]

expr :: Parser Expr
expr = makeExprParser term operators

newtype ParseErr = ParseErr (ParseError (Token String) Void)

instance Show ParseErr where
    show (ParseErr e) = parseErrorPretty e

instance Exception ParseErr

parseProgram :: (MonadThrow m) => String -> String -> m [Stmt]
parseProgram name s = either (throwM . ParseErr) return (parse stmts name s)
