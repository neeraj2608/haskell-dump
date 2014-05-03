{-
- File finding
-}
module FileFind where
 
import System.Directory (getDirectoryContents, doesDirectoryExist,
                           getPermissions, Permissions, getModificationTime)
import System.FilePath ((</>), takeExtension)
import Control.Monad
import Data.Time (UTCTime(..))
import System.IO
import Control.Exception (SomeException(..), handle, bracket)

type InfoP a = FilePath -> Permissions -> Maybe Integer -> UTCTime -> a

type Predicate = InfoP Bool

pathPredicate :: InfoP FilePath
pathPredicate path _ _ _ = path

sizePredicate :: InfoP Integer
sizePredicate _ _ (Just size) _ = size
sizePredicate _ _ Nothing _ = -1

makePredicate :: Eq a => InfoP a -> a -> Predicate
-- w x y z are the args of f. e.g. f could be pathP
--makePredicate f k = \w x y z -> f w x y z == k
makePredicate f k w x y z = f w x y z == k

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

findWithBetterPred :: Predicate -> FilePath -> IO [FilePath]
findWithBetterPred p x = do
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
            -- release function. Will be exec'ed even if use function
            -- exceptions out
            hClose
            -- use function
            $ (\fHandle ->
            do
                size <- hFileSize fHandle
                return (Just size))

main :: IO [FilePath]
--main = findWithSimplePred (\x -> takeExtension x == ".hs") "."
main = findWithBetterPred (makePredicate sizePredicate 2914) "."
