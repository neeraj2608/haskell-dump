{-
- File finding
-}
module FileFind where
 
import System.Directory (getDirectoryContents, doesDirectoryExist,
                         getPermissions, getModificationTime)
import System.FilePath ((</>), takeExtension)
import Control.Monad
import System.IO
import Control.Exception (SomeException(..), handle, bracket)

import Predicates
import PredicateCombinators

-- strict find
listFiles :: FilePath -> IO [FilePath]
listFiles currentDir = do
    filesDirs <- liftM removeCurrentAndParentDir $ getDirectoryContents currentDir
    results <- liftM concat $ mapM recurseListFiles filesDirs
    filterM notEmpty results
    where
        removeCurrentAndParentDir :: [FilePath] -> [FilePath]
        removeCurrentAndParentDir = filter (`notElem` [".",".."])

        recurseListFiles :: FilePath -> IO [FilePath]
        recurseListFiles y = do
            let fullPath = currentDir </> y
            isDirectory <- doesDirectoryExist fullPath
            if isDirectory
              then listFiles fullPath
            else return [fullPath]

        notEmpty :: FilePath -> IO Bool
        notEmpty = return . not . null

findWithSimplePred :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
findWithSimplePred p x = do
    result <- listFiles x 
    filterM checkPred result
    where
       checkPred :: FilePath -> IO Bool
       checkPred = return . p

findWithPred :: Predicate -> FilePath -> IO [FilePath]
findWithPred p x = do
    result <- listFiles x 
    filterM checkPred result
    where
        checkPred :: FilePath -> IO Bool
        checkPred file = do
            perm <- getPermissions file
            size <- getFileSize file
            time <- getModificationTime file
            return (p file perm size time)

-- catches exceptions while opening files
getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize x = handle (\(SomeException _) -> return Nothing) $
    -- catch exceptions from hFileSize on named pipes
    -- and close those file handles
    bracket (openFile x ReadMode)
            -- release. Will be exec'ed even if use function
            -- exceptions out
            hClose
            -- use.
            $ (\fHandle ->
            do
                size <- hFileSize fHandle
                return (Just size))

main :: IO [FilePath]
main = findWithPred ((liftPath takeExtension ==? ".hs")
                           &&?
                           (sizePredicate <? 3000)) "."

--main = findWithPred (makePredicate sizePredicate (==) 3114) "."
--main = findWithPred (equalPredicate sizePredicate 3046) "."
--main = findWithSimplePred (\x -> takeExtension x == ".hs") "."
