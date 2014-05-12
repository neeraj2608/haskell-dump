{-
- Based on Learn you a haskell - Non-monadic Zippers for Lists
-}
module Zipper where

data List a = Empty |
              Cons (List a)
              deriving (Show)
              
type Zipper a = ([a],[a]) --fst = focused list, snd = crumbs

(<->) :: a -> (a -> b) -> b
(<->) x y = y x

testList :: [Int]
testList = [3,4,2,5,6,8,9,1,2]

goForward :: Zipper a -> Zipper a
goForward (x:xs,y) = (xs,x:y)

goBackward :: Zipper a -> Zipper a
goBackward (xs,y:ys) = (y:xs,ys)