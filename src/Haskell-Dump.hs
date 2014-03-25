module HaskellDump where

{-
Messing about with Haskell.
-}

{----------------------
GUARDS
-----------------------}
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

{----------------------
PATTERNS
-----------------------}
-- find head of list
head' :: [a] -> a
head' [] = error "No head for empty list"
head' (x:_) = x

{----------------------
CASE
-----------------------}
-- find head of list
head'' :: [a] -> a
head'' x = case x of [] -> error "No head for empty list"
                     x:_ -> x

{----------------------
RECURSION
-----------------------}
-- return max of list
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
take'' _ [] = []
take'' _ [x] = [x]
take'' 0 _ = []
take'' x (y:ys) = [y] ++ take'' (x-1) ys

-- reverse a list
reverse'' :: [a] -> [a]
reverse'' [] = []
reverse'' (x:xs) = reverse'' xs ++ [x]

-- return last but one element
lastButOne :: [a] -> a
lastButOne [] = error "empty list"
lastButOne [x] = error "list too short"
lastButOne [x,y] = x
lastButOne (x:xs) = lastButOne xs 

{----------------------
TYPE DEFINITIONS
-----------------------}
data BookInfo = Book Int String
                deriving (Show, Eq)
           
type CustomerId = Int -- type aliasing

type CustomerName = String -- type aliasing

data CustomerInfo = Customer CustomerId CustomerName
                    | String -- alternative value constructors 
                    deriving (Show, Eq)      

a = Customer 1 "a"
b = Book 1 "a"
-- a == b will return false even though they are structurally equivalent (composed of an Int and a String
-- with the same corresponding values) coz they're different types
-- naked tuples do NOT provide this protection:
-- (1,2) == (1,2) returns True
-- Using tuples in constructing a data type will still give us this type safety, though:

data BookInfo' = Book' (Int, String)
                 deriving (Show, Eq)
                 
data CustomerInfo' = Customer' (Int, String)
                     deriving (Show, Eq)
                     
a' = Customer' (1,"a")
b' = Book' (1,"a")
-- a' == b' will still return False

