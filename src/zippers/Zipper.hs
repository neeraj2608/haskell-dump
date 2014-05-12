{-
- Based on Learn you a haskell - Non-monadic Zippers for Lists
-}
module Zipper where

data List a = Empty |
              Cons (List a)
              deriving (Show)
              
type Zipper = ([a],[a]) --everything except the hole