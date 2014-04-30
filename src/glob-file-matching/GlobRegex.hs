module GlobRegex(fileNameMatches) where

import Data.Char (toLower)
import Text.Regex.Posix ((=~))

globToRegex :: String -> String
globToRegex x = '^' : globToRegex' x ++ "$"

globToRegex' :: String -> String
globToRegex' "" = ""
globToRegex' ('*':xs) = ".*" ++ globToRegex' xs
globToRegex' ('?':xs) = "." ++ globToRegex' xs -- '?' matches single char
globToRegex' ('[':'!':x:xs) = "[^" ++ x : characterClass xs
globToRegex' ('[':x:xs) = '[' : x : characterClass xs
globToRegex' "[" = error "Unterminated character class."
globToRegex' (x:xs) = escapeSpecialChars x ++ globToRegex' xs

characterClass :: String -> String
characterClass (']':xs) = ']' : globToRegex' xs
characterClass (x:xs) = x : characterClass xs
characterClass "" = error "Unterminated character class."

escapeSpecialChars :: Char -> String
escapeSpecialChars x | x `elem` "\\{}|^+()$.]" = '\\' : [x]
                     | otherwise = [x]

fileNameMatches :: FilePath -> String -> Bool -> Bool
fileNameMatches x y ignoreCase = map f x =~ globToRegex (map f y) :: Bool
                                 where f | ignoreCase = Data.Char.toLower
                                         | otherwise = id
