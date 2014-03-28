module HaskellDump where

import Data.Maybe
import Data.List

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
aa = "a"
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

-- pattern matching works on user-defined types
-- printName' is called an accessor
-- must start lowercase!
printName' (Customer' (a, _)) = a

-- Combining accessors and type definitions
data BookInfo'' = Book'' {
                          bookId :: Int,
                          bookName :: String
                         }
                         deriving (Show, Eq)

-- using the accessor
a'' = Book'' 1 "a"

-- using the accessor
-- we can change the order of data in this way of doing things
a1' = Book'' {bookName="a", bookId=1}

-----------------
-- parameterized types
-----------------
data Maybe' b = Just' b | Nothing'
               deriving (Show)

aMaybe = Just' True
aMaybe' = Nothing'

-----------------
-- recursive types
-----------------
data List a = Cons a (List a) -- List is defined in terms of itself
              | Nil
              deriving (Show, Eq)

la = Nil -- :type lA gives List a
lb = Cons 1 la -- :type lb gives List Integer
lc = Cons 1 lb -- :type lc gives List Integer

-- convert to List
-- NOTE that Nil is a value constructor that we defined. It is NOT an inbuilt type.
toList (x:xs) = Cons x (toList xs)
toList [] = Nil

-- convert from List
fromList (Cons a b) = a : (fromList b)
fromList Nil = []

-- binary tree
data Tree a = Node a (Tree a) (Tree a)
            | Empty
            deriving (Show, Eq)

aTree = Node 1 (Node 2 Empty Empty) (Node 3 Empty Empty)
bTree = Empty

-- binary tree constructor using Maybe
data Tree' a = Node' a (Maybe (Tree' a)) (Maybe (Tree' a))

aTree' = Node' 1 (Nothing) (Nothing)
bTree' = Node' 1 (Just (Node' 2 Nothing Nothing)) Nothing

----------------
-- useful errors
----------------
takeSecond (x:xs) | (null xs) = error "too short a list"
                  | otherwise = (head xs) 

x1 = takeSecond [1,2,3] -- good
x2 = takeSecond [1] -- throws exception

-- printSecond uses takeSecond but if takeSecond throws an
-- exception, printSecond can't do anything with it (bcoz
-- the exception exits IMMEDIATELY
printSecond x = takeSecond x

z1 = printSecond [1,2,3] -- all good, z1 = 2
z2 = printSecond [1] -- exception, z2 can't be assigned a value

safeSecond (_:x:_) = Just x
safeSecond _ = Nothing

safePrintSecond x = if(safeSecond x == Nothing) then -1
                    else fromJust (safeSecond x)

z3 = safePrintSecond [1,2,3] -- z3 will be 2
z4 = safePrintSecond [1] -- z4 will be equal to -1 and will actually have a value (unlike when an exception was being thrown by takeSecond)

{----------------------
LOCAL VARIABLES
-----------------------}
-----------------------
-- let
-----------------------

lend amount = let currentBalance = 100
                  balance = currentBalance - amount
                  in if(amount > currentBalance)
                     then Nothing
                     else Just balance -- not calculated unless the else is entered
                     
-- lets can be nested
fx = let a = 1
     in let b = 3
        in a+b
        
-- nesting can cause shadowing
fz = let a = 1
     in let b = 3
            a = 5 -- this a takes effect
        in a+b
        
-- wheres are like lets
fy = a + b
     where a = 1
           b = 3
           
-- lets can define functions. syntax same as variables.
fa a = let b = 1
       in let blah a = a + b -- blah is a function
          in blah a
          
-- num elements in list
numElems :: [a] -> Int
numElems [] = 0
numElems (x:xs) = 1 + numElems(xs)

-- mean of list
-- we traverse the list twice (once in numElems and once in sumList)
meanList [] = 0
meanList x = sumList x / (fromIntegral (numElems x))
             where sumList [] = 0
                   sumList (x:xs) = x + sumList(xs)

-- mean of list
calcMean x = meanList' x 0 0                   
meanList' [] sum numElems | numElems /= 0 = sum / (fromIntegral numElems)
                          | otherwise = 0
meanList' (x:xs) sum numElems = meanList' xs (sum+x) (numElems+1)

-- palindrome of list
palList [] = []
palList (x:xs) = [x] ++ palList xs ++ [x]

-- is list a palindrome?
-- version 1
isPalList [] = False
isPalList [x] = True
isPalList (x:xs) | (x == last xs) = True && if(null (init xs))
                                            then True
                                            else isPalList (init xs)
                 | otherwise = False

-- version 2
isPalList' [] = False
isPalList' [x] = True
isPalList' [x,y] = x == y
isPalList' (x:xs) = (x == last xs) && isPalList' (init xs)

-- intersperse :: a -> [[a]] -> [a]
-- e.g. intersperse ',' ["foo","bar"]
-- gives "foo","bar"
intersperse :: a -> [[a]] -> [a]
intersperse _ [] = []
intersperse _ ([x]) = x -- matching a single list
intersperse y (x:z) = x ++ [y] ++ HaskellDump.intersperse y z -- here, x matches a list and z matches a LIST of LISTS

-- binary tree height
-- using max
binTreeHeight Empty = 0
binTreeHeight (Node a lChild rChild) = 1 + max (binTreeHeight lChild)(binTreeHeight rChild)
                                     
testTree = Node 1 (Node 2 Empty Empty) (Node 3 Empty (Node 4 Empty Empty))

{--
LIST MANIPULATION
--}
pre = "burra" `isPrefixOf` "burrarum" -- true
inf = "rar" `isInfixOf` "burrarum" -- true
suf = "rum" `isSuffixOf` "burrarum" -- true

-- head, init, tail, last exception if []

-- concat
conc = concat [[1,2,3],[4,5]] -- gives [1,2,3,4,5]

-- reverse
rev = reverse [4,3,2,1]

-- and and or
true = and [True, True]
false = and [True, True, False]
true' = or [True, False]
false' = or [False, False]

-- and gives true iff no members are false
true'' = and [] -- this is True!!

-- or gives true iff at least one member is true
false'' = or [] -- this is False

-- all and any
true''' = all even [2,4,6]
false''' = any odd [2,4,6]

blah = [1,2,3,4]
y = take 2 blah -- 1,2
x = drop 2 blah -- 3,4

z = splitAt 2 blah -- 0 to 1 goes into first list; 2 to end goes into second list. z = ([1,2],[3,4]). splitAt = take + drop

blah' = [1,3,5,4,2]
z' = takeWhile odd blah' -- [1,3,5]
z'' = dropWhile odd blah' -- [4,2]

z''' = span odd blah' -- ([1,3,5],[4,2])
z'''' = break even blah' -- ([1,3,5],[4,2])

z_ = elem 1 blah' -- True
z__ = filter odd blah' ++ [7] -- [1,3,5,7]

z___ = zip blah blah' -- [(1,1),(2,3),(3,5),(4,4)]. stops at shorter length
z_' = zipWith (+) blah blah' -- [2,5,8,8] zip + map

z__' = words "abc    bcd b      eed" -- splits on whitespace
z_'' = unwords ["abc","bcd","b","eed"] -- "abc bcd b eed" adds a single space between each

z_''' = unlines ["abc","bcd","b","eed"] -- there's a \n at the end!

-- Safe versions of head, tail, last and init
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead a = Just (head a)

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail a = Just (tail a)

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit a = Just (init a)

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast a = Just (last a)

-- takes a predicate and a list of any type, and splits its input list
-- on every element for which the predicate returns False.
-- e.g. splitWith odd [2,4,6,1,8,10,5,12] gives [[2,4,6],8,10],[12]]

-- version 1
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith predicate list = let bs = case b of
                                         (x:y) -> y
                                         x     -> x
                           in [a] ++ splitWith predicate bs
                           where (a,b) = break predicate list
                         
-- version 2
splitWith' :: (a -> Bool) -> [a] -> [[a]]
splitWith' _ [] = []
splitWith' predicate list = [x] ++ splitWith' predicate (drop (length x + ((length list) - (length y))) list)
                            where x = takeWhile (not . predicate) y
                                  y = dropWhile predicate list
                            
-- version 3
splitWith'' :: (a -> Bool) -> [a] -> [[a]]
splitWith'' _ [] = []
splitWith'' predicate list = [x] ++ splitWith'' predicate (dropWhile predicate y)
                             where (x,y) = span (not . predicate) (dropWhile predicate list)