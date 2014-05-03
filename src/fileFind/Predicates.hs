module Predicates where

import System.Directory (Permissions)
import Data.Time (UTCTime(..))

type InfoP a = FilePath -> Permissions -> Maybe Integer -> UTCTime -> a

type Predicate = InfoP Bool

pathPredicate :: InfoP FilePath
pathPredicate path _ _ _ = path

sizePredicate :: InfoP Integer
sizePredicate _ _ (Just size) _ = size
sizePredicate _ _ Nothing _ = -1

--equalPredicate :: Eq a => InfoP a -> a -> Predicate
---- w x y z are the args of f. e.g. f could be pathP
----equalPredicate f k = \w x y z -> f w x y z == k
--equalPredicate f k w x y z = f w x y z == k

--makePredicate :: Eq a => InfoP a -> (a -> a -> Bool) -> a -> Predicate
--makePredicate f op k w x y z =  f w x y z `op` k
