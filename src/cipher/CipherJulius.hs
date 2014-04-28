{-
- Playing with encryption
-}
shiftAZ :: Int -> Char -> Char
shiftAZ c a = int2Letter $ (letter2Int a + c) `mod` 26

letter2Int :: Char -> Int
letter2Int x = fromEnum x - fromEnum 'a'

int2Letter :: Int -> Char
int2Letter x = toEnum(toEnum (x + fromEnum 'a'))

cipher :: String -> String
cipher = map (shiftAZ 3)

decipher :: String -> String
decipher = map (shiftAZ (-3))

main :: IO ()
main = print $ (decipher . cipher) "zzz"
