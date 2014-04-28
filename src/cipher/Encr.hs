module Encr where

import EncrUtilities

encipher :: Int -> String -> String -> String
encipher blockSize key = cipher blockSize (symFromBlock key)

decipher :: Int -> String -> String -> String
decipher blockSize key = cipher blockSize (- symFromBlock key)

cipher :: Int -> Integer -> String -> String
cipher blockSize keySymbol = concat .
                             map blockFromSym . -- convert back to blocks with plain-alphabet symbols
                             map shiftSymbol . -- encipher
                             map symFromBlock . -- convert to cipher-alphabet symbols
                             fixedSizeBlocks blockSize ' '
                             where
                               shiftSymbol x = (x + keySymbol) `mod` cipherAlphabetSize
                               cipherAlphabetSize = numeralBase ^ blockSize

-- pads blocks with padChar if the block size is smaller than blockSize
fixedSizeBlocks :: Int -> Char -> String -> [String]
fixedSizeBlocks _ _ [] = []
fixedSizeBlocks bSize padChar x | bSize > (length x) = [x ++ (replicate (bSize - (length x)) padChar)]
                                | otherwise = [take bSize x] ++ fixedSizeBlocks bSize padChar (drop bSize x)

maximDijkstra, ciphertext, plaintext :: String
maximDijkstra = "Besides a mathematical inclination, an exceptionally\n" ++
                "good mastery of one's native tongue is the most vital\n" ++
                "asset of a competent programmer.\n"

ciphertext = encipher blockSize key maximDijkstra
plaintext = decipher blockSize key ciphertext

main :: IO ()
main = do
         putStrLn ciphertext
         putStrLn plaintext

key :: String
key = "computing science"
blockSize :: Int
blockSize = 10
