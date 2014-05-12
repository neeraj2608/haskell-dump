module Types where

import Test.QuickCheck
import Control.Monad (liftM2, liftM3)

data Tree a = Empty |
              Node a (Tree a) (Tree a)
              deriving (Show)

data Direction = L |
                 R
                 deriving (Show)

type Directions = [Direction]

newtype BreadCrumbs a = BreadCrumbs {getCrumbs :: ([Direction], [a])}

newtype Zipper a = Zipper {getZip :: (BreadCrumbs a, a)}

getNode :: Zipper a -> a
getNode = snd . getZip

instance Monad Zipper where
    return x = Zipper (BreadCrumbs ([], []), x)
    x >>= makeX = Zipper (BreadCrumbs (d++ds, t), t')
        where
            (d, t) = getCrumbs b
            (b, t') = getZip $ makeX y
            (BreadCrumbs (ds, _), y) = getZip x

type Name = String
type Data = String
data FSItem = File Name Data |
              Folder Name [FSItem]
              deriving (Show, Eq)

data FSCrumb = FSCrumb Name [FSItem] [FSItem]
               deriving (Show, Eq)

type FSZipper = ([FSCrumb], FSItem)

instance Arbitrary FSItem where
    arbitrary = oneof [liftM2 Folder arbitrary list,
                       liftM2 File arbitrary arbitrary]

-- limit recursion
-- http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html
list :: Gen [FSItem]
list = sized list'

list' :: Int -> Gen [FSItem]
list' 0 = return []
list' n = oneof [(\x -> return [x]) =<< liftM2 Folder arbitrary sublist,
                 (\x -> return [x]) =<< liftM2 File arbitrary arbitrary]
          where sublist = list' (n `div` 2)

instance Arbitrary FSCrumb where
    arbitrary = liftM3 FSCrumb arbitrary arbitrary arbitrary
