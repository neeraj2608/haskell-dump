{-
- Based on Programming in Haskell, Section 10.4
-}
module TautologyCheck where

import Data.List (sort, group)
import Control.Applicative ((<$>))

-------------------------------------------------------------------------------
data Token = And Token Token |
             Implication Token Token |
             Not Token |
             Var String |
             Constant Bool

type Table = [(String, Bool)]

-------------------------------------------------------------------------------
main :: IO ()
main = do
    let checkThis = [p1, p2, p3, p4]
    putStrLn $ unwords $ map f checkThis
    where
        f x = do
                let varTable = (removeDups . addToVarTable) x
                let result = isTautology x varTable
                if result
                    then "Yes"
                    else "No"

p1 = And (Var "x") (Not (Var "x")) -- (a ^ !a) (not a tautology)
p2 = Implication (And (Var "a") (Var "b")) (Var "a") -- a^b => a (not a tautology)
p3 = Implication (Var "a") (Var "a") -- a => a (a tautology)
p4 = Implication (And (Var "b") (Implication (Var "a") (Var "a"))) (Var "b") -- (b ^ (a=>a)) => b (a tautology)
-------------------------------------------------------------------------------
removeDups :: [String] -> [String]
removeDups = map head . group . sort -- O(nlogn)

addToVarTable :: Token -> [String]
addToVarTable (And x y) = addToVarTable x ++ addToVarTable y
addToVarTable (Implication x y) = addToVarTable x ++ addToVarTable y
addToVarTable (Constant x) = []
addToVarTable (Not x) = addToVarTable x
addToVarTable (Var x) = [x]

-------------------------------------------------------------------------------
isTautology :: Token -> [String] -> Bool
isTautology x varTable = all (==True) $ evalToken x <$> createPermutations varTable

-------------------------------------------------------------------------------
createPermutations :: [String] -> [Table]
createPermutations x = zip x <$> boolPermuts (length x)

boolPermuts :: Int -> [[Bool]]
boolPermuts n | n<=1 = [[True], [False]]
              | otherwise = [y:x | y <- [True, False], x <- boolPermuts (n-1)]

-------------------------------------------------------------------------------
evalToken :: Token -> Table -> Bool
evalToken (And x y) table = and [evalToken x table, evalToken y table]
evalToken (Implication x y) table = evalToken x table == evalToken y table
evalToken (Constant x) _ = x
evalToken (Not x) table = not $ evalToken x table
evalToken (Var x) table = lookUp x table -- O(n)

lookUp :: String -> Table -> Bool
lookUp x (y:ys) | x == fst y = snd y
                | otherwise = lookUp x ys
lookUp x [] = error $ x ++ " not found"