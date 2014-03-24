module Test where

{-
Learning Haskell.
-}

-- guards
-- find bmi given height and weight pair
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
        | bmi < 5 = "Crap"
        | otherwise = "OK"
        where bmi = weight/height^2

-- find max of list
max' :: (Ord a) => a -> a -> a
max' x y
        | x < y = y
        | otherwise = x

-- determing ordering of pair of elems
myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
        | a > b = GT
        | a < b = LT
        | otherwise = EQ

-- print first char of two strings
initials :: String -> String -> String
initials (x:_) (y:_) = [x] ++ "" ++ [y]

f = 4 * (let a = 9 in a + 1) -- let clause is an expression in itself.

g = let square x = x ^ 2 in [square 3, square 5]

-- list comprehension
-- calculate bmi of multiple height-weight pairs
calcBmi :: (RealFloat a) => [(a, a)] -> [a]
calcBmi x = [bmi | (w, h) <- x, let bmi = w/h^2]

-- pattern
-- find head of list
head' :: [a] -> a
head' [] = error "No head for empty list"
head' (x:_) = x

-- case example
-- find head of list
head'' :: [a] -> a
head'' x = case x of [] -> error "No head for empty list"
                     x:_ -> x

-- recursion
max'' :: (Ord a) => [a] -> a
max'' [] = error "No max for empty list"
max'' [x] = x
max'' (x:y) | x > max'' y = x
            | otherwise = max'' y 

-- duplicate something a given no. of times
replicate' :: (Num a, Ord a) => a -> b -> [b]
replicate' x y | x <= 0 = []
               | otherwise = y : replicate' (x-1) y

-- take a certain number of elements from start of list