module EncrUtilities(
                     blockFromSym,
                     symFromBlock,
                     numeralBase) where

symFromBlock :: String -> Integer
symFromBlock x = integerFromNumerals numeralBase $ map integerCodeFromChar x

integerFromNumerals :: Integer -> [Integer] -> Integer
integerFromNumerals b x = foldl multadd 0 x
                          where multadd a xs = b*a + xs

blockFromSym :: Integer -> String
blockFromSym x = map charFromIntegerCode $ numeralsFromInteger numeralBase x

numeralsFromInteger :: Integer -> Integer -> [Integer]
numeralsFromInteger _ 0 = []
numeralsFromInteger b x = numeralsFromInteger b d ++ [r]
                          where myDiv ds = ds `divMod` b
                                (d, r) = myDiv x

integerCodeFromChar :: Char -> Integer
integerCodeFromChar x | x == ' ' = fromIntegral (minCode-1)
                      | x == '\n' = fromIntegral (minCode-2)
                      | fromEnum x > maxCode || fromEnum x < minCode = error "Only printable ASCII chars, space and newline allowed."
                      | otherwise = fromIntegral (fromEnum x - minCode)

charFromIntegerCode :: Integer -> Char
charFromIntegerCode x | x == fromIntegral minCode-1 = ' '
                      | x == fromIntegral minCode-2 = '\n'
                      | otherwise = toEnum (fromIntegral x + minCode)

maxCode, minCode, numExtraCodes :: Int
maxCode = 126 -- set of code-characters =
minCode = 32 -- {tab, newline, toEnum 32 ... toEnum 126}
numExtraCodes = 2

numeralBase :: Integer
numeralBase = fromIntegral(maxCode - minCode + 1 + numExtraCodes)
