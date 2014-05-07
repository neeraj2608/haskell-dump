module ImageParse where

import Data.ByteString.Lazy as L
import Data.Int (Int64)
import Data.Word (Word8)
import Data.Char (chr)
import Control.Applicative ((<$>))

-------------------------------------------------------------------------------
data ParseState = ParseState {
                      residual :: L.ByteString,
                      offset :: Int64
                  }

newtype Parser parseResult = Parser {
                      runParser :: ParseState -> Either String (parseResult, ParseState)
                  }

-------------------------------------------------------------------------------
-- bind for Parsers
(==>) :: Parser a -> (a -> Parser b) -> Parser b
(==>) firstParser makeSecondParser = Parser (\secondState -> case runParser firstParser secondState of
                                         Left err -> Left err
                                         Right (firstResult, _) -> (runParser . makeSecondParser) firstResult secondState)

-------------------------------------------------------------------------------        
makeParserFromResult :: a -> Parser a
makeParserFromResult x = Parser (\s -> Right (x, s))

-------------------------------------------------------------------------------
instance Functor Parser where
  fmap f p = p ==> (\x -> makeParserFromResult (f x))

-------------------------------------------------------------------------------
parse :: Parser a -> L.ByteString -> Either String a
parse p state = case runParser p $ ParseState state 0 of
                    Left err -> Left err
                    Right (a, _) -> Right a

-------------------------------------------------------------------------------
parseWord8 :: Parser Word8
parseWord8 = getParserState ==>
            \oldState -> case L.uncons (residual oldState) of
                      Nothing -> Parser $ (\s -> Left $ "error at offset " ++ (show $ offset s))
                      Just y@(curWord, remainingString) -> Parser $ (\s -> Right (curWord, newState))
                        where newState = oldState {residual = remainingString, offset = offset oldState + 1}
                        
peekByte :: Parser (Maybe Word8)
peekByte = (fmap fst . L.uncons . residual) <$> getParserState

-------------------------------------------------------------------------------
-- Use functor properties to do other parses
parseChar :: Parser Char
parseChar = fmap word82Char parseWord8

word82Char :: Word8 -> Char
word82Char = chr . fromIntegral

peekChar :: Parser (Maybe Char)
peekChar = (<$>) word82Char `fmap` peekByte -- the `fmap` takes ((<$>) word82Char) into the Maybe
-------------------------------------------------------------------------------
parseWhile :: (Word8 -> Bool) -> Parse [Word8]
parseWhile  = undefined

-------------------------------------------------------------------------------
getParserState :: Parser ParseState
getParserState = Parser (\x -> Right (x, x))