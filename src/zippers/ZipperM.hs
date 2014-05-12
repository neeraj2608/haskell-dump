{-
- Based on Learn you a haskell - Zippers
- Changed zipper to monad so we can chain them like parsers.
-}
module ZipperM where

import Types

-------------------------------------------------------------------------------
main :: IO()
main = print $ test testTree

test :: Tree Char -> Tree Char
test x = getNode $ snd $ getZip $
    goLeft (return x) 
    >>= goUp
    >>= goRight
    >>= modify (const 'P')
    >>= goLeft
    >>= goLeft
    >>= goLeft
    >>= goToRoot

testTree :: Tree Char  
testTree =   
    Node 'A'  
        (Node 'B'  
            (Node 'C'  
                (Node 'D' Empty Empty)  
                (Node 'E' Empty Empty)  
            )  
            (Node 'F'  
                (Node 'G' Empty Empty)  
                (Node 'H' Empty Empty)  
            )  
        )  
        (Node 'I'  
            (Node 'J'  
                (Node 'K' Empty Empty)  
                (Node 'L' Empty Empty)  
            )  
            (Node 'M'  
                (Node 'N' Empty Empty)  
                (Node 'O' Empty Empty)  
            )  
        )

-------------------------------------------------------------------------------
-- traversal
-------------------------------------------------------------------------------
elemAt :: Directions -> Tree a -> a
elemAt _ Empty = error "Empty tree"
elemAt (L:ds) (Node x y z) = elemAt ds y
elemAt (R:ds) (Node x y z) = elemAt ds z
elemAt [] (Node x _ _) = x

goLeft :: Zipper (Tree a) -> Zipper (Zipper (Tree a))
goLeft (Zipper (BreadCrumbs b, Node x y z)) = return $ Zipper (BreadCrumbs (L:fst b, Node x Empty z:snd b), y)
goLeft x@(Zipper (BreadCrumbs b, Empty)) = return x

goRight :: Zipper (Tree a) -> Zipper (Zipper (Tree a))
goRight (Zipper (BreadCrumbs b, Node x y z)) = return $ Zipper (BreadCrumbs (R:fst b, Node x y Empty:snd b), z)
goRight x@(Zipper (BreadCrumbs b, Empty)) = return x

goUp :: Zipper (Tree a) -> Zipper (Zipper (Tree a))
goUp (Zipper (BreadCrumbs (L:ds, Node x _ y:as), n)) = return $ Zipper (BreadCrumbs (ds, as), Node x n y)
goUp (Zipper (BreadCrumbs (R:ds, Node x y _:as), n)) = return $ Zipper (BreadCrumbs (ds, as), Node x y n)
goUp x@(Zipper (BreadCrumbs (_, []), n)) = return x

goToRoot :: Zipper (Tree a) -> Zipper (Zipper (Tree a))
goToRoot x@(Zipper (BreadCrumbs (_, []), n)) = return x
goToRoot x = goUp x >>= goToRoot

-------------------------------------------------------------------------------
-- modifications
-------------------------------------------------------------------------------
modify :: (a -> a) -> Zipper (Tree a) -> Zipper (Zipper (Tree a))
modify f (Zipper (BreadCrumbs a, Node x y z)) = return $ Zipper (BreadCrumbs a, Node (f x) y z)
modify f x@(Zipper (BreadCrumbs a, Empty)) = return x

insert :: Tree a -> Zipper (Tree a) -> Zipper (Zipper (Tree a))
insert = undefined