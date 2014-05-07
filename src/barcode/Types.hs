module Types where

import Control.Applicative ((<$>))
import Data.Array

-------------------------------------------------------------------------------
lCodes = [
                     -- encoding digit:
          "0001101", -- 0
          "0011001", -- 1
          "0010011", -- 2
          "0111101", -- 3
          "0100011", -- 4
          "0110001", -- 5
          "0101111", -- 6
          "0111011", -- 7
          "0110111", -- 8
          "0001011"  -- 9
         ]

rCodes = map complement <$> lCodes
    where
        complement :: Char -> Char
        complement '1' = '0'
        complement '0' = '1'
        
gCodes = reverse <$> rCodes

lOrG = [ -- l = 0, g = 1
                   -- first digit =
         "000000", -- 0
         "001011", -- 1
         "001101", -- 2
         "001110", -- 3
         "010011", -- 4
         "011001", -- 5
         "011100", -- 6
         "010101", -- 7
         "010110", -- 8
         "011010"  -- 9
       ]
       
toArray :: [a] -> Array Int a
toArray x = listArray (0, length x - 1) x

-------------------------------------------------------------------------------
lArray = toArray lCodes
rArray = toArray rCodes
gArray = toArray gCodes
lOrGArray = toArray lOrG