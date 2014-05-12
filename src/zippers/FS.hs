{-
- Filesystem using Zippers, based on learn you a haskell - zippers
- We use monadic zippers here
- For usage examples, see FSTest.hs
-}
module FS where

import Types

-------------------------------------------------------------------------------
getName :: FSItem -> Name
getName (File x _) = x
getName (Folder x _) = x

-------------------------------------------------------------------------------
-- FS functionality
-------------------------------------------------------------------------------
fsUp :: FSZipper -> Maybe FSZipper
fsUp (FSCrumb name before after:xs, y) = Just (xs, Folder name (before++[y]++after))
fsUp x@([], _) = Just x

fsDown :: Name -> FSZipper -> Maybe FSZipper
fsDown _ x@(_, File _ _) = Just x
fsDown toName (xs, Folder name items) = case after of
    [] -> Nothing
    _ -> Just (y:xs, z)
    where
        z = head after
        y = FSCrumb name before (tail after)
        (before,after) = break matchName items

        matchName :: (FSItem -> Bool)
        matchName x = (==) toName $ getName x

fsRename :: Name -> FSZipper -> Maybe FSZipper
fsRename newName (x, File _ y) = Just (x, File newName y)
fsRename newName (x, Folder _ y) = Just (x, Folder newName y)

fsNewFile :: FSItem -> FSZipper -> Maybe FSZipper
fsNewFile _ x@(_, File _ _) = Just x
fsNewFile w (z, Folder x y) = Just (z, Folder x (w:y))
