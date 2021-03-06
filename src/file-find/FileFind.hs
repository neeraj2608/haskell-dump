{-
- File finding
-}
module FileFind where
 
import System.Directory (getDirectoryContents, doesDirectoryExist,
                         getPermissions, getModificationTime, searchable)
import System.FilePath ((</>), takeExtension)
import Control.Monad
import System.IO
import Control.Exception (SomeException(..), handle, bracket)

import Predicates
import PredicateCombinators

-- traverse with folds and customizable iterators
data Iterate x = Done {unwrap :: x} |
                 Skip {unwrap :: x} |
                 Continue {unwrap :: x}

type Iterator x = x -> Info -> Iterate x

foldTraverse :: Iterator a -> a -> FilePath -> IO a
foldTraverse iter x currentDir = do
    filesDirs <- liftM removeCurrentAndParentDir $ getDirectoryContents currentDir
    infos <- mapM (getInfo . (currentDir </>)) filesDirs
    foldWalk iter x infos
    where
        foldWalk it z (y:ys) = case it z y of
            Done m -> return m
            Skip _ -> foldWalk it z ys
            Continue m -> do
                if isDirectory y
                    then foldTraverse it m (path y)
                    else foldWalk it m ys
        foldWalk _ z _ = return z

removeCurrentAndParentDir :: [FilePath] -> [FilePath]
removeCurrentAndParentDir = filter (`notElem` [".",".."])

countDirsIterator :: Iterator Integer
countDirsIterator count info = Continue (if isDirectory info
                                             then count+1
                                             else count)

countFilesIterator :: Iterator Integer
countFilesIterator count _ = Continue (count+1)

isDirectory :: Info -> Bool
isDirectory x = searchable p
    where
        (Just p) = perms x

-- user-controlled filesystem walks
-- reorder resorts the file list however the user wants it
-- this is a strict function
traverse :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverse reorder currentDir = do
    filesDirs <- liftM removeCurrentAndParentDir $ getDirectoryContents currentDir
    infos <- mapM (getInfo . (currentDir </>)) filesDirs
    liftM concat $ forM (reorder infos) $ (\info -> do
         if isDirectory info
             then traverse reorder (path info)
             else return [info])

traverseWithPred :: ([Info] -> [Info]) -> Predicate -> FilePath -> IO [FilePath]
traverseWithPred reorder p currentDir = do
    infos <- traverse reorder currentDir
    liftM (map path) $ filterM f infos
    where
        f :: Info -> IO Bool
        f = (\info -> do
                let Just pe = perms info
                let Just ti = modTime info
                return $ p (path info) pe (size info) ti)

getInfo :: FilePath -> IO Info
getInfo file = do
    perm <- getPermissions file
    size <- getFileSize file
    time <- getModificationTime file
    return (Info file (Just perm) size (Just time))

-- strict find
listFiles :: FilePath -> IO [FilePath]
listFiles currentDir = do
    filesDirs <- liftM removeCurrentAndParentDir $ getDirectoryContents currentDir
    results <- liftM concat $ mapM recurseListFiles filesDirs
    filterM notEmpty results
    where
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

test4 :: IO Integer
test4 = foldTraverse countDirsIterator 0 "."

test3 :: IO [FilePath]
test3 = traverseWithPred reverse ((liftPath takeExtension ==? ".hs")
                                  &&?
                                  (sizePredicate <? 3000)) "."

test2 :: IO [Info]
test2 = traverse reverse "."

test1 :: IO [FilePath]
test1 = findWithPred ((liftPath takeExtension ==? ".hs")
                      &&?
                      (sizePredicate <? 3000)) "."

--main = findWithPred (makePredicate sizePredicate (==) 3114) "."
--main = findWithPred (equalPredicate sizePredicate 3046) "."
--main = findWithSimplePred (\x -> takeExtension x == ".hs") "."
