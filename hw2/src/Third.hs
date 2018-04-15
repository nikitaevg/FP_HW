module Third where

import Control.Applicative (Alternative, empty, many, some, (<|>))
import Control.Monad (join, (>=>))
import Data.Bifunctor (first)
import Data.Char (digitToInt, isDigit, isSpace)
import Data.List (isPrefixOf)

newtype Parser s a = Parser {runParser :: [s] -> Maybe (a, [s]) }

instance Functor (Parser s) where
    fmap f (Parser parser) = Parser (fmap (first f) . parser)

instance Applicative (Parser s) where
    pure x = Parser $ \s -> Just (x, s)
    Parser pf <*> Parser pa = Parser $ pf >=> \(f, s) ->
                                       pa s >>= \(a, left) ->
                                       Just (f a, left)

instance Monad (Parser s) where
    return = pure
    (Parser p) >>= f = Parser (\s -> join $ (uncurry runParser) <$> (first f <$> p s))

instance Alternative (Parser s) where
    empty = Parser (const Nothing)
    (Parser a) <|> (Parser b) = Parser (\s -> (a s) <|> (b s))

ok :: Parser a ()
ok = Parser $ \s -> Just ((), s)

eof :: Parser a ()
eof = Parser $ \s -> case s of
    [] -> Just ((), [])
    _  -> Nothing

satisfy :: (a -> Bool) -> Parser a a
satisfy p = Parser $ \s -> case s of
    []     -> Nothing
    (x:xs) -> if p x then Just (x, xs) else Nothing

element :: Eq a => a -> Parser a a
element c = satisfy (== c)

parseDigit :: Parser Char Int
parseDigit = digitToInt <$> (satisfy isDigit)

stream :: Eq a => [a] -> Parser a [a]
stream str = Parser $ \s -> if isPrefixOf str s
                            then Just (str, snd $ splitAt (length str) s)
                            else Nothing

trim :: Parser Char String
trim = many $ satisfy isSpace

parseBrackets :: Parser Char String
parseBrackets = trim *>
                (concat <$> many ((\a l b -> a:l ++ [b])
                            <$> element '(' <*> parseBrackets <*> element ')'))
                <* trim

parseNum :: Parser Char Int
parseNum = trim *> ((((*) (-1)) <$ element '-') <|>
           id <$ element '+' <|>
           id <$ ok) <*>
           ((listToInt <$> some parseDigit)) <* trim
           where
             listToInt = foldl1 (\acc x -> acc * 10 + x)

parseList :: Parser Char [[Int]]
parseList = ((:) <$> listP <*> many (element ',' *> listP)) <|> (const []) <$> ok where
                parseN :: Int -> Parser Char [Int]
                parseN 1 = (return) <$> (parseNum)
                parseN i = (:) <$> (parseNum <* element ',') <*> parseN (i - 1)
                listP :: Parser Char [Int]
                listP = (parseNum <* element ',') >>= parseN
