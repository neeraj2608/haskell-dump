module Glob where

import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory, getDirectoryContents)
import Control.Monad
import System.FilePath ((</>), dropTrailingPathSeparator, splitFileName)

matchingNames :: FilePath -> IO [FilePath]
matchingNames patternToMatch | isPattern patternToMatch = let x = splitFileName patternToMatch
                                                          in    
                                                            case x of
                                                              ("", b) -> do
                                                                           currentDir <- getCurrentDirectory
                                                                           listMatches currentDir b
                                                              (a,b) -> if isPattern a
                                                                         then matchingNames a
                                                                         else undefined
                             | otherwise = do
                                             exists <- doesFileOrDirectoryExist patternToMatch
                                             if exists
                                               then return [patternToMatch]
                                               else return []
matchingNames [] = return []

listMatches :: FilePath -> String -> IO [FilePath]
listMatches dir pat = undefined

listFiles :: IO [FilePath]
listFiles = liftM (filter (not . flip elem [".",".."])) $ getDirectoryContents =<< getCurrentDirectory

doesFileOrDirectoryExist :: FilePath -> IO Bool
doesFileOrDirectoryExist patternToMatch = do
                                           fileExists <- doesFileExist patternToMatch
                                           if fileExists
                                             then return fileExists
                                             else doesDirectoryExist patternToMatch

isPattern :: String -> Bool
isPattern = any (`elem` "?*[")
