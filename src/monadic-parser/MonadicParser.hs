{-
- From the paper Functional Pearls - Monadic Parsing in Haskell
- http://www.cs.nott.ac.uk/~gmh/pearl.pdf
-}
module MonadicParser where

import Control.Monad hiding (MonadPlus)
import Data.List as L
import Data.Char (isSpace)

main :: IO ()
main = undefined

-- non-empty list in result represents success
-- fst is the result of parsing so far
-- snd is the unparsed suffix
-- lists allow returning multiple alternatives if
-- the grammar is ambiguous
newtype Parser a = Parser {runParser :: String -> [(a, String)]} 

instance Monad Parser where
    return x = Parser (\y -> [(x,y)])
    (>>=) firstParser makeSecondParser = Parser (\secondString -> concat $ [(runParser . makeSecondParser) a b |
                                                                            (a,b) <- runParser firstParser secondString])

-- (++) and zero form a monoid
class Monad m => MonadZero m where
    zero :: m a

instance MonadZero Parser where
    zero = Parser (\_ -> []) -- represents a failure

class MonadZero m => MonadPlus m where
    (++) :: m a -> m a -> m a
    
-- (++) represents a non-deterministic choice operator since it evaluates
-- both the LH and RH parsers
instance MonadPlus Parser where
    (++) (Parser a) (Parser b) = Parser (\x -> (a x) L.++ (b x)) -- join results of both parsers

-- (+++) is a non-deterministic choice (same as (++)) from the result of
-- which we extract only the first element
(+++) :: Parser a -> Parser a -> Parser a
(+++) a b = Parser (\x -> case (runParser $ (MonadicParser.++) a b) x of
                              [] -> []
                              (x:xs) -> [x])

-- consume one character of input
item :: Parser Char
item = Parser (\cs -> case cs of
                          "" -> []
                          (x:xs) -> [(x,xs)])

-- conditional parsing
condItem :: (Char -> Bool) -> Parser Char
condItem cond = do{c<-item; if cond c then return c else zero}

char :: Char -> Parser Char
char c = condItem (c==)

space :: Parser String
space = many (condItem isSpace)

token :: Parser a -> Parser a
token p = do{a<-p;space;return a}

symbol :: String -> Parser String
symbol s = token (string s)

apply :: Parser a -> String -> [(a, String)]
apply p = runParser (do{space; p})

string :: String -> Parser String
string "" = return ""
string (c:cs) = do{char c; string cs; return (c:cs)}

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do{a<-p; as<-many p; return (a:as)}

sepBy :: Parser a -> Parser b -> Parser [a]
p `sepBy` sep = (p `sepBy'` sep) +++ return []

sepBy' :: Parser a -> Parser b -> Parser [a]
p `sepBy'` sep = do{a<-p;as<-many (do{sep;p;});return (a:as)}

chainl :: Parser a -> Parser (a->a->a) -> a -> Parser a
chainl p op a = p `chainl'` op +++ return a

chainl' :: Parser a -> Parser (a->a->a) -> Parser a
p `chainl'` op = do{a<-p;f a}
                     where
                         f a = (do{b<-p;c<-op;f (c a b)})+++(return a)

chainr :: Parser a -> Parser (a->a->a) -> a -> Parser a
chainr p op a = p `chainr'` op +++ return a

chainr' :: Parser a -> Parser (a->a->a) -> Parser a
p `chainr'` op = x where
                     x = p >>= f
                     f a = (do{c<-op;b<-x;return (c a b)})+++(return a)