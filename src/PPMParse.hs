module PPMParse where

import Data.ByteString.Lazy as L
import Data.Int (Int64)
import Data.Word (Word8)
import Data.Char (chr, isDigit, isSpace)
import Control.Applicative ((<$>))
import Data.List as Li
import Data.Array

-------------------------------------------------------------------------------
data ParseState = ParseState {
                      residual :: L.ByteString,
                      offset :: Int64
                  }

newtype Parser parseResult = Parser {
                      runParser :: ParseState -> Either String (parseResult, ParseState)
                  }
                  
type Pixel = Word8
type RGBPixel = (Pixel, Pixel, Pixel)
type Image = Array (Int, Int) RGBPixel

-------------------------------------------------------------------------------
-- bind >>= for Parsers
(=>>=) :: Parser a -> (a -> Parser b) -> Parser b
(=>>=) firstParser makeSecondParser = Parser (\secondState -> case runParser firstParser secondState of
                                         Left err -> Left err
                                         Right (firstResult, _) -> (runParser . makeSecondParser) firstResult secondState)

-- then >> for Parsers
(=>>) :: Parser a -> Parser b -> Parser b
(=>>) firstParser secondParser = firstParser =>>= \_ -> secondParser

-------------------------------------------------------------------------------        
toParser :: a -> Parser a
toParser x = Parser (\s -> Right (x, s))

-------------------------------------------------------------------------------
instance Functor Parser where
  fmap f p = p =>>= (\x -> toParser (f x))
  
-------------------------------------------------------------------------------
ppmParse :: Parser Image
ppmParse = parseWithWhile word82Char (not . isSpace) =>>=
           \magic -> skipWhiteSpace =>> assert (magic /= "P6") "Invalid header" =>>
           parseInt =>>= \width ->  skipWhiteSpace =>>
           parseInt =>>= \height ->  skipWhiteSpace =>>
           parseInt =>>= \maxVal -> assert (maxVal /= 255) "Value /= 255" =>>
           parseChar =>>= \singleWhiteSpace -> assert((not . isSpace) singleWhiteSpace) "Expected a single whitespace" =>>
           parseTimes (width * height) parseRGBPixel =>>= \rgbPixel ->
           toParser (listArray ((0,0),(width-1, height-1)) rgbPixel)

assert :: Bool -> String -> Parser ()
assert f s | f = Parser (\_ -> Left s)
           | otherwise = toParser ()

skipWhiteSpace :: Parser ()
skipWhiteSpace = parseWithWhile word82Char isSpace =>> toParser ()

parseTimes :: Int -> Parser a -> Parser [a]
parseTimes 0 _ = toParser []
parseTimes n p = p =>>= \b -> (b:) <$> parseTimes (n-1) p

parseRGBPixel :: Parser RGBPixel
parseRGBPixel = parseWord8 =>>= \red ->
                parseWord8 =>>= \green ->
                parseWord8 =>>= \blue ->
                toParser (red, green, blue)

toGrayScale :: RGBPixel -> Pixel
toGrayScale (r, g, b) = round $ (fromIntegral r)*0.30 + (fromIntegral g)*0.59 + (fromIntegral b)*0.11 

data Bit = Zero | One

toMonochrome :: Ix i => Double -> Array i Pixel -> Array i Bit
toMonochrome threshValue array = f <$> array
    where
        f x | fromIntegral x < pivot = Zero
            | otherwise = One
        pivot = round $ min + (max - min) * threshValue
        min = fromIntegral $ foldArray lt array
        max = fromIntegral $ foldArray gt array
        
        gt x y | x > y = y
               | otherwise = x
        
        lt x y | x < y = x
               | otherwise = y
        
        foldArray :: Ix i => (a -> a -> a) -> Array i a -> a
        foldArray f arr = foldArray' f (arr ! (fst . bounds) arr) arr
        
        foldArray' :: Ix i => (a -> a -> a) -> a -> Array i a -> a
        foldArray' f acc arr = f' f acc (indices arr)
            where
                  f' _ x [] = x
                  f' f x (y:ys) = f' f (f x (arr ! y)) ys

-------------------------------------------------------------------------------
parse :: Parser a -> L.ByteString -> Either String a
parse p state = case runParser p $ ParseState state 0 of
                    Left err -> Left err
                    Right (a, _) -> Right a

-------------------------------------------------------------------------------
parseWord8 :: Parser Word8
parseWord8 = getParserState =>>=
            \oldState -> case L.uncons (residual oldState) of
                      Nothing -> Parser $ (\s -> Left $ "error at offset " ++ (show $ offset s))
                      Just y@(curWord, remainingString) -> Parser $ (\s -> Right (curWord, newState))
                        where newState = oldState {residual = remainingString, offset = offset oldState + 1}
                        
peekByte :: Parser (Maybe Word8)
peekByte = (fmap fst . L.uncons . residual) <$> getParserState

-- Use functor properties to do other parses
parseChar :: Parser Char
parseChar = fmap word82Char parseWord8

word82Char :: Word8 -> Char
word82Char = chr . fromIntegral

peekChar :: Parser (Maybe Char)
peekChar = (<$>) word82Char `fmap` peekByte -- the `fmap` takes ((<$>) word82Char) into the Maybe

parseInt :: Parser Int
parseInt = parseWithWhile word82Char isDigit =>>= \digitArray ->
    if Li.null digitArray
        then Parser (\_ -> Left "No more input")
        else let n = read digitArray
            in if n < 0
                then Parser (\_ -> Left "Overflow")
                else toParser n

-------------------------------------------------------------------------------
parseWhile :: (Word8 -> Bool) -> Parser [Word8]
parseWhile p = maybeBoolParser =>>= \maybeBool ->
   case maybeBool of
       Just True -> parseWord8 =>>= \b -> ([b] ++) <$> parseWhile p -- parseWord8 has updated the ParseState so the next application of parseWhile p
                                                                    -- is being applied to a different state (may not be readily apparent from the 
                                                                    -- function application call)
       Just False -> toParser []
   where
      maybeBoolParser :: Parser (Maybe Bool)
      maybeBoolParser = fmap p `fmap` peekByte

parseWithWhile :: (Word8 -> a) -> (a -> Bool) -> Parser [a]
parseWithWhile p f = fmap p `fmap` parseWhile (f . p)

-------------------------------------------------------------------------------
getParserState :: Parser ParseState
getParserState = Parser (\x -> Right (x, x))