{-
- EAN 13 Barcode recognition
-}
module Barcode where

import Types
import Data.Char (digitToInt)
import Data.Array

-------------------------------------------------------------------------------
checkDigit :: Integral a => [a] -> a
checkDigit x = 10 - (mod sumEveryOtherDigit 10)
    where
        -- EAN-13: odd positions get weight 3, even get weight 1
        sumEveryOtherDigit = sum $ zipWith (\a b -> b a) x $ cycle [id, (*3)]

-------------------------------------------------------------------------------
encodeDigits :: String -> String
encodeDigits = f . map digitToInt
    where
        f :: [Int] -> String
        f xs@(x:_) = leadGuard ++
                     constructLeftGroup (lOrGArray ! x) leftSix ++
                     centerGuard ++
                     constructRightGroup rightSix ++
                     trailGuard
                     where
                         leftSix = (drop 1 . fst) y
                         rightSix = snd y
                         y = splitAt 7 xs
                         leadGuard = "101"
                         centerGuard = "01010"
                         trailGuard = "101"
        
        constructLeftGroup :: String -> [Int] -> String
        constructLeftGroup lOrGPattern digsToEncode = concat $ map encLDigit $ zip lOrGPattern digsToEncode
        
        encLDigit :: (Char, Int) -> String
        encLDigit (x, y) | x == '0' = lArray ! y
                         | otherwise = encRDigit y
        
        constructRightGroup :: [Int] -> String
        constructRightGroup digsToEncode = concat $ map encRDigit digsToEncode
        
        encRDigit :: Int -> String
        encRDigit dig = rArray ! dig 

main :: IO ()
main = undefined