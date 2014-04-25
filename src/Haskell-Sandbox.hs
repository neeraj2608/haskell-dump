module HaskellDump where

import Data.Maybe
import Data.List
import Data.Char
import Data.Bits
import Data.Either
import System.IO

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
                             
-- convert string to digit
sToDigit :: String -> Int
sToDigit s = loop 0 s

loop :: Int -> String -> Int
loop acc [] = acc
loop acc (x:xs) = let acc' = 10*acc + digitToInt x
                  in loop acc' xs
                  
-- square every element in list
sqElem :: [Int] -> [Int]
sqElem [] = []
sqElem (x:xs) = x^2 : sqElem xs

sqElem' x = map square x
            where square x = x * x

-- convert every letter to uppercase
toUpperCase [] = []
toUpperCase (x:xs) = toUpper x : toUpperCase xs

-- select even elems
evenElems [] = []
evenElems (x:xs) | even x = x : evenElems xs
                 | otherwise = evenElems xs

evenElems'  x = filter even x

{--
FOLDS
--}
-- foldl defn
foldl'' :: (a -> b -> a) -> a -> [b] -> a
foldl'' step zero (x:xs) = foldl'' step (step zero x) xs
foldl'' _ zero [] = zero

-- foldr defn
foldr'' :: (b -> a -> a) -> a -> [b] -> a
foldr'' step zero (x:xs) = step x (foldr step zero xs)
foldr'' step zero [] = zero

-- foldl using foldr
foldLR :: (a -> b -> a) -> a -> [b] -> a
foldLR step zero (x:xs) = foldr step' (foldLR step zero xs) [x]
                          where step' x y = step y x
foldLR step zero [] = zero 

-- foldl using foldr - their version - very interesting
myFoldl :: (a -> b -> a) -> a -> [b] -> a

myFoldl f z xs = foldr step id xs z
    where step x g a = g (f a x)
    
-- this is how it evaluates for xs = [1,2], z = 0 i.e.,
-- foldr step id [1,2] 0
-- (step 1 (foldr step id [2])) 0 -- coz function application is left-associative
-- (step 1 (step 2 (foldr step id []))) 0
-- (step 1 (step 2 id)) 0
-- (step 1 (id (+2 ?))) 0 -- ? signifies a partial function waiting for an input to the ?
-- (id (+2 ?)) (+1 ?) 0
-- (+2 ?) (+1 ?) 0
-- (+2 (+1 ?)) 0
-- (+2 +1 ?) 0
-- (0 + 2 + 1)
-- = 3

-- identity with foldr
identity :: [a] -> [a]
identity x = foldr (:) [] x

-- append with foldr
append :: [a] -> [a] -> [a]
append listToAppend appendToThisList = foldr (:) listToAppend appendToThisList

-- sum of list no fold
calcSum x = sumList 0 x
            where sumList :: Int -> [Int] -> Int
                  sumList sum [] = sum
                  sumList sum (x:xs) = sumList (sum + x) xs 
                  
-- sum of list using foldl
calcSum' x = foldl (+) 0 x

-- Adler checksum no fold
calcChecksum x = adlerChecksum 1 0 x
                 where adlerChecksum a b []     = (b `shiftL` 16) .|. a
                       adlerChecksum a b (x:xs) = adlerChecksum a' b' xs
                                                  where a' = (a + (ord x .&. 0xFF)) `mod` base
                                                        b' = (b + a) `mod` base
                                                        base = 65521

-- Adler checksum using foldl
calcChecksum' x = let (a,b) = foldl func (1,0) x
                  in (b `shiftL` 16) .|. a
                  where func (a,b) x = let a' = (a + (ord x .&. 0xFF)) `mod` base
                                       in (a', (b + a') `mod` base)
                        base = 65521  

-- filter using no folds
filter'' :: (a -> Bool) -> [a] -> [a]
filter'' predicate (x:xs) | predicate x = x : filter'' predicate xs
                          | otherwise = filter'' predicate xs
filter'' _ [] = []

-- filter with folds
-- foldl
filterLFold pr x = foldl pred [] x
                   where pred y x | pr x = y ++ [x]
                                  | otherwise = y
-- foldr
filterRFold pr  x = foldr pred [] x
                    where pred xs acc | pr xs = xs : acc
                                      | otherwise = acc

-- map without folds
map'' f (x:xs) = f x : map'' f xs
map'' _ _ = []

-- map with folds
-- foldl
mapLFold f x = foldl f' [] x
               where f' acc x = acc ++ [f x]
               
-- foldr
mapRFold f x = foldr f' [] x
               where f' x acc = (f x) : acc

-- convert string to int using folds
asInt_fold :: String -> Int
asInt_fold "" = 0
asInt_fold "-" = 0
asInt_fold s'@(s:ss) | s == '-' = (-1) * (foldl f 0 ss)
                     | otherwise = foldl f 0 s' 
                     where f acc x = let acc' = acc*10 + digitToInt x
                                     in detect_int_overflow(acc',acc)
                                     where detect_int_overflow (x,y) | x < 0 && y < 0 || x > 0 && y < 0 = error "Int Overflow"
                                                                     | otherwise = x

-- convert string to int using folds. safer version
-- this is probably buggy!!
type ErrorMessage = String

saferDigitToInt :: Char -> Either ErrorMessage Int
saferDigitToInt x | x `elem` ['0'..'9'] = Right (digitToInt x)
                  | otherwise = Left "Not a digit." 

asInt_either :: String -> Either ErrorMessage Int
asInt_either "" = Right 0
asInt_either "-" = Right 0
asInt_either s' = foldl f (Right 0) s' 
                  where f (Right acc) x = let acc' = acc*10 + digitToInt x
                                          in detect_int_overflow(acc',acc)
                                          where detect_int_overflow (x,y) | x < 0 && y < 0 || x > 0 && y < 0 = Left "Int Overflow"
                                                                          | otherwise = Right x

-- concat using foldr
concatFR :: [[a]] -> [a]
concatFR x = foldr f [] x
             where f x acc = x ++ acc
             
-- takewhile without foldr
takeWhile' _ [] = []
takeWhile' p (x:xs) | p x = x : takeWhile' p xs
                    | otherwise = []
                  
-- takewhile with foldr
takeWhileFR p x = foldr f [] x
                  where f x acc | p x = x : acc
                                | otherwise = []

-- groupBy with foldl
-- since we use foldl, this does NOT work on infinite lists
-- (e.g. test2 below) and hence is NOT a valid groupBy replacement
groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' _ [] = []
groupBy' p x = foldl f [[]] x
               where f acc x | null (last acc) = [[x]]
                             | p (head (last acc)) x = (init acc) ++ [(last acc) ++ [x]]
                             | otherwise = acc ++ [[x]]
{--
test1 f = (f (<) [1,2,3,2,0,0,3,3,1,0]) == (groupBy (<) [1,2,3,2,0,0,3,3,1,0])
test2 f = (head . head . f (<) $ [1..]) == (head . head . groupBy (<) $ [1..])
test3 f = (take 100 $ f (==) $ cycle [1,2,2,1]) == (take 100 $ groupBy (==) $ cycle [1,2,2,1])
test4 f = (head . head $ f (==) $ repeat 1) == (head . head $ groupBy (==) $ repeat 1)
test5 f = (f (==) $ (take 100000 $ cycle [1,2,2,1])) == (groupBy (==) $ (take 100000 $ cycle [1,2,2,1]))
test6 f = (f (==) []) == (groupBy (==) [])
test7 f = (f (==) [1]) == (groupBy (==) [1])
testAll f = map (\t -> t f) [test1,test2,test3,test4,test5,test6,test7]
--}

-- any with fold
-- foldl
anyFL :: (a -> Bool) -> [a] -> Bool
anyFL p x = foldl f False x
            where f acc x | p x = True
                          | otherwise = acc
                         
-- foldr
anyFR :: (a -> Bool) -> [a] -> Bool
anyFR p x = foldr f False x
            where f x acc | p x = True
                          | otherwise = acc

-- words with fold
-- foldl
wordsFL :: String -> [String]
wordsFL s = foldl f [[]] s
            where f :: [String] -> Char -> [String]
                  f acc x | isSpace x = case (last acc) of 
                                          [] -> acc
                                          _  -> acc ++ [[]]
                          | otherwise = (init acc) ++ [(last acc) ++ [x]]

-- foldr
wordsFR :: String -> [String]
wordsFR s = foldr f [[]] s
            where f x acc | isSpace x = case (head acc) of
                                          [] -> acc
                                          _  -> [[]] ++ acc
                          | otherwise = [[x] ++ (head acc)] ++ (tail acc)

-- unlines with fold
-- foldl
unlinesFL :: [String] -> String
unlinesFL s = foldl f "" s
              where f acc x = acc ++ x ++ "\n"
              
-- foldr
unlinesFR :: [String] -> String
unlinesFR s = foldr f "" s
              where f x acc = x ++ "\n" ++ acc

-- cycle with fold
-- foldr
cycleFR :: [a] -> [a]
cycleFR x = foldr f cycleFR x x
            where f x y z = x:(y z)

{--
LAMBDAS
--}
isInAny needle haystack = any checkNeedle haystack
                          where checkNeedle x = isInfixOf needle x  
                          
isInAny2 needle haystack = any (\s -> isInfixOf needle s) haystack

{--
PARTIAL FUNCTIONS
--}
-- using partial functions
nicerSum :: [Integer] -> Integer
nicerSum = foldl (+) 0 -- mind == blown!

isInAny3 :: Eq a => [a] -> [[a]] -> Bool
isInAny3 needle = any (isInfixOf needle)                          
                          
{--
SECTIONS
--}
powerOfTwo x = map (2^) x

isLowerCase = (`elem` ['a'..'z']) -- backticks are indispensable here because we're fixing the SECOND argument of elem

{--
AS PATTERN
--}
-- as patterns enable data sharing
nonEmptyTails x'@(_:xs) = x' : nonEmptyTails xs -- if we'd used (_:xs) on the RHS, that data would have to be copied over from
                                                -- wherever it was stored for the LHS
nonEmptyTails [] = []

{--
COMPOSITION
--}
-- (.) is right associative!
compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

nonEmptyTails' x = compose init tails x

nonEmptyTails'' = compose init tails -- with currying

nonEmptyTails''' = init . tails

-- count num words in string that start with capital letter
selectCapitalWords = filter isCap
                     where isCap (x:xs) | x `elem` ['A'..'Z'] = True
                           isCap _ = False
countCapitalWords = length . selectCapitalWords . words

countCapitalWords' = length . filter (isUpper . head) . words

-- tail recursion - complete generality, longer to read and understand
-- folds - between tail recursion and list manipulation in terms of generality. folds in themselves do very
--         specific things but the step functions let us make them do whatever the context requires, thus
--         making them a little more general
-- list manipulation function - completely specific (one task per function), quicker to read and understand

{--
Typeclasses
--}
-- class definition
class Basiceq a where
  isEq :: a -> a -> Bool
  
-- instance definition
instance Basiceq Bool where
  isEq True True = True
  isEq False False = True
  isEq _ _ = False
  
-- default method definitions
class Basiceq2 a where
  isEq2, isNotEq2 :: a -> a -> Bool
  
  -- default definitions
  isEq2 x y = not (isNotEq2 x y)
  isNotEq2 x y = not (isEq2 x y)
  
instance Basiceq2 Bool where
  -- must provide at least one implementation or you have infinite loop!!
  isEq2 True True = True
  isEq2 False False = True
  isEq2 _ _ = False
  
data Color = Red | Green | Blue

instance Basiceq2 Color where
  isEq2 Red Red = True
  isEq2 Green Green = True
  isEq2 Blue Blue = True
  isEq2 _ _ = False
  
-- important built-in typeclasses
-- Show. Defines function show.
-- putStr vs show
-- ghci prints a value as it would be entered into a Haskell program. so adds extra quotes + escaping
-- exapmle show 1 gives "1". putStr 1 gives 1.

instance Show Color where
  show Red = "R"
  show Green = "G"
  show Blue = "B"
  
-- Read. Defines function read which takes a string and converts it to the requisite data type
enterNumber :: IO ()
enterNumber = do
          putStrLn "Enter a number"
          inputStr <- getLine
          let i = (read inputStr)::Double -- we have to specify which data type we expect or compiler will try to infer!
          putStrLn ("You entered "++(show i))

-- Actions
str2message :: String -> String -- pure
str2message x = "Data " ++ x

str2action :: String -> IO ()
str2action = putStrLn . str2message

printNumbers1to10 :: IO ()
printNumbers1to10 = do
                 mapM_ (str2action . show) ([1..10]::[Int])

-- (>>=)
printName :: IO ()
printName = putStrLn "Enter your name" >>
            getLine >>= (\enteredString -> putStrLn $ "You entered " ++ enteredString)

isGreen :: IO Bool
isGreen = do
            putStrLn "Is green your favorite color?"
            inpStr <- getLine
            return $ (toUpper $ head inpStr) == 'Y'

-- file read and writes
printFileContentsInUpperCase :: String -> IO ()
printFileContentsInUpperCase x = do
                                   inputFileHandle <- openFile x ReadMode
                                   inputFileContents <- hGetContents inputFileHandle
                                   let result = map toUpper inputFileContents
                                   putStrLn result
                                   hClose inputFileHandle
