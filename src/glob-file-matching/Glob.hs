module Glob where
{-
- Glob search for file names
-}

import System.Directory (doesDirectoryExist, getCurrentDirectory, getDirectoryContents)
import Control.Monad
import System.FilePath ((</>), splitFileName)
import GlobRegex (fileNameMatches)

findMatches :: FilePath -> IO ()
findMatches patternToMatch = do
                               matches <- matchingNames patternToMatch
                               (putStr . unlines) matches

matchingNames :: FilePath -> IO [FilePath]
matchingNames patternToMatch = do
                                 currentDir <- getCurrentDirectory
                                 searchRecursive currentDir patternToMatch

searchRecursive :: FilePath -> String -> IO [FilePath]
searchRecursive currentDir patternToMatch = do
                                 fileAndDirList <- listFiles currentDir
                                 dirs <- filterM doesDirectoryExist $ map (currentDir </>) fileAndDirList
                                 childrenFiles <- mapM (`searchRecursive` patternToMatch) dirs
                                 let currentDirFiles = filter (\x -> fileNameMatches (snd $ splitFileName x) patternToMatch True) fileAndDirList
                                 return $ map (currentDir </>) currentDirFiles ++ concat childrenFiles

listFiles :: FilePath -> IO [FilePath]
listFiles currentDir = liftM (filter (not . flip elem [".",".."])) $ getDirectoryContents currentDir

isPattern :: String -> Bool
isPattern = any (`elem` "?*[")
