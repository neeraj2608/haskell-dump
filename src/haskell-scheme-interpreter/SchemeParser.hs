module SchemeParser where

import System.Environment
import Text.ParserCombinators.Parsec
import Control.Monad (liftM)

data SchemeVal = Atom String |
                 String String |
                 Bool Bool |
                 Number Integer |
                 DottedList [SchemeVal] SchemeVal |
                 List [SchemeVal]
                 deriving (Show)

main :: IO ()
main = do
         args <- getArgs
         putStrLn $ readExpr $ head args

specialSymbols :: Parser Char
specialSymbols = oneOf "!$% &|*+-/:<=?>@^_~#"

readExpr :: String -> String
readExpr input = case parse parseExpr "" input of
                   Left err -> "No match " ++ show err
                   Right a -> show a

ignoreSpaces :: Parser ()
ignoreSpaces = skipMany1 space

parseExpr :: Parser SchemeVal
parseExpr = parseAtom <|> parseDigits <|> parseString

parseAtom :: Parser SchemeVal
parseAtom = do
              first <- letter <|> specialSymbols
              rest <- many (letter <|> specialSymbols <|> digit)
              let atom = first : rest
              case atom of
                "#t" -> return $ Bool True
                "#f" -> return $ Bool False
                _ -> return $ Atom atom

parseDigits :: Parser SchemeVal
parseDigits = liftM (Number . read) $ many1 digit

parseString :: Parser SchemeVal
parseString = do
                char '"'
                x <- many $ noneOf "\""
                char '"'
                return $ String x
