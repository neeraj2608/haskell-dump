module Types where

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
            (BreadCrumbs (ds, tOld), y) = getZip x
            
type Name = String
type Data = String
data FSItem = File Name Data |
              Folder Name [FSItem]
              deriving (Show)
            
data FSCrumb = FSCrumb Name [FSItem] [FSItem]
               deriving (Show)

type FSZipper = ([FSCrumb], FSItem)