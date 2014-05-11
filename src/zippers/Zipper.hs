{-
- From Learn you a haskell - Zippers
-}
module Zipper where

main :: IO()
main = undefined

data Tree a = Empty |
              Node a (Tree a) (Tree a)
              deriving (Show)
              
freeTree :: Tree Char  
freeTree =   
    Node 'P'  
        (Node 'O'  
            (Node 'L'  
                (Node 'N' Empty Empty)  
                (Node 'T' Empty Empty)  
            )  
            (Node 'Y'  
                (Node 'S' Empty Empty)  
                (Node 'A' Empty Empty)  
            )  
        )  
        (Node 'L'  
            (Node 'W'  
                (Node 'C' Empty Empty)  
                (Node 'R' Empty Empty)  
            )  
            (Node 'A'  
                (Node 'A' Empty Empty)  
                (Node 'C' Empty Empty)  
            )  
        )
        
changeWtoP' :: Tree Char -> Tree Char
changeWtoP' (Node x l (Node m (Node _ a b) n)) = (Node x l (Node m (Node 'P' a b) n))
changeWtoP' Empty = Empty

data Direction = L |
                 R
                 deriving (Show)
                 
type Directions = [Direction]

changeWtoP :: Directions -> Tree Char -> Tree Char
changeWtoP _ Empty = Empty
changeWtoP (L:ds) (Node x y z) = Node x (changeWtoP ds y) z
changeWtoP (R:ds) (Node x y z) = Node x y (changeWtoP ds z)
changeWtoP [] (Node _ y z) = Node 'P' y z

elemAt :: Directions -> Tree a -> a
elemAt _ Empty = error "Empty tree"
elemAt (L:ds) (Node x y z) = elemAt ds y
elemAt (R:ds) (Node x y z) = elemAt ds z
elemAt [] (Node x _ _) = x

newtype BreadCrumbs a = BreadCrumbs {getCrumbs :: ([Direction], Maybe a)}

newtype Zipper a = Zipper {getZip :: (BreadCrumbs a, a)}

instance Monad Zipper where
    return x = Zipper (BreadCrumbs ([], Nothing), x)
    x >>= makeX = Zipper (BreadCrumbs (d++ds, t), t')
        where
            (d,t) = getCrumbs b
            (b, t') = getZip $ makeX y
            (BreadCrumbs (ds, _), y) = getZip x

goLeft  :: Tree a -> Zipper (Tree a)
goLeft (Node x y z) = Zipper (BreadCrumbs ([L], Just (Node x Empty z)), y)

goRight :: Tree a -> Zipper (Tree a)
goRight (Node x y z) = Zipper (BreadCrumbs ([R], Just (Node x y Empty)), z)

twoLefts :: Tree Char -> Tree Char
twoLefts x = snd $ getZip $ (goLeft x >>= goRight)