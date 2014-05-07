module ImageParse where

import Data.ByteString.Lazy as L
import Data.Int (Int64)
import Data.Word (Word8)

data ParseState = ParseState {
                      residual :: L.ByteString,
                      offset :: Int64
                  }
                  
newtype Parse a = Parse {
                      runParse :: ParseState -> Either String (a, ParseState)
                  }
                  
idParser :: a -> Parse a
idParser x = Parse (\s -> Right (x, s))

parse :: Parse a -> L.ByteString -> Either String a
parse p state = case runParse p $ ParseState state 0 of
                    Left err -> Left err
                    Right (a, _) -> Right a
                    
parseByte :: Parse Word8
parseByte = undefined