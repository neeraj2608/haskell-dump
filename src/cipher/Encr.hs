module Encr where

import EncrUtilities

encipher :: Int -> String -> String -> String
encipher bSize key = cipher bSize (symFromBlock key)

decipher :: Int -> String -> String -> String
decipher bSize key = cipher bSize (- symFromBlock key)

cipher :: Int -> Integer -> String -> String
cipher bSize keySymbol = concat .
                         map blockFromSym . -- convert back to blocks with plain-alphabet symbols
                         map shiftSymbol . -- encipher
                         map symFromBlock . -- convert to cipher-alphabet symbols
                         fixedSizeBlocks bSize ' '
                         where
                           shiftSymbol x = (x + keySymbol) `mod` cipherAlphabetSize
                           cipherAlphabetSize = numeralBase ^ bSize

-- pads blocks with padChar if the block size is smaller than blockSize
fixedSizeBlocks :: Int -> Char -> String -> [String]
fixedSizeBlocks _ _ [] = []
fixedSizeBlocks bSize padChar x | bSize > length x = [x ++ replicate (bSize - length x) padChar]
                                | otherwise = take bSize x : fixedSizeBlocks bSize padChar (drop bSize x)

maximDijkstra :: String
maximDijkstra = "Besides a mathematical inclination, an exceptionally\n" ++
                "good mastery of one's native tongue is the most vital\n" ++
                "asset of a competent programmer.\n"

k :: String
k = "computing science"

blockSize :: Int
blockSize = 10

main :: IO ()
main = do
         let ciphertext = encipher blockSize k maximDijkstra
         let plaintext = decipher blockSize k ciphertext
         putStrLn $ "Cipher text: " ++ ciphertext
         putStr $ "Plain text: " ++ plaintext
