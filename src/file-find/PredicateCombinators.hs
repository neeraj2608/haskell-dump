module PredicateCombinators (
    (&&?),
    (||?),
    (==?),
    (>?),
    (<?),
    liftPath) where

import Predicates (InfoP, Predicate)

liftDoublePredicate :: Ord a => (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftDoublePredicate op f k w x y z = f w x y z `op` k w x y z

liftSinglePredicate :: Ord a => (a -> b -> c) -> InfoP a -> b -> InfoP c
liftSinglePredicate op f k = liftDoublePredicate op f (toInfoP k)

liftPath :: (FilePath -> a) -> InfoP a
liftPath op w = toInfoP (op w) w

toInfoP :: b -> InfoP b
toInfoP b _ _ _ _ = b

(&&?) :: Predicate -> Predicate -> Predicate
(&&?) = andPredicate
infixr 3 &&?

andPredicate :: Predicate -> Predicate -> Predicate
andPredicate = liftDoublePredicate (&&)

(||?) :: Predicate -> Predicate -> Predicate
(||?) = orPredicate
infixr 2 ||?

orPredicate :: Predicate -> Predicate -> Predicate
orPredicate = liftDoublePredicate (||)

(==?) :: Ord a => InfoP a -> a -> Predicate
(==?) = equalPredicate
infix 4 ==?

equalPredicate :: Ord a => InfoP a -> a -> Predicate
equalPredicate = liftSinglePredicate (==)

(>?) :: Ord a => InfoP a -> a -> Predicate
(>?) = greaterPredicate
infix 4 >?

greaterPredicate :: Ord a => InfoP a -> a -> Predicate
greaterPredicate = liftSinglePredicate (>)

(<?) :: Ord a => InfoP a -> a -> Predicate
(<?) = lesserPredicate
infix 4 <?

lesserPredicate :: Ord a => InfoP a -> a -> Predicate
lesserPredicate = liftSinglePredicate (<)
