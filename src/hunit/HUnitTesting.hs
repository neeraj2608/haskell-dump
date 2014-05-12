module HUNitTesting where

import Test.QuickCheck

main :: IO()
main = do
        quickCheck (prop_idempotent :: [Integer] -> Bool)
        quickCheck (prop_minimum' :: [Integer] -> Property)

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
    where lhs = filter  (< x) xs
          rhs = filter (>= x) xs

prop_idempotent xs = qsort (qsort xs) == qsort xs

prop_minimum' xs = not (null xs) ==> head (qsort xs) == minimum xs
