-- From http://www.cs.ou.edu/cs1323h
module SequenceUtilities
    (allEqual,
     apply,
     blocks,
     blocksRigid,
     centerInField,
     concatWithSpacer,
     decreasing,
     decreasingStrictly,
     dropFromRight,
     dropWhileFromRight,
     increasing,
     increasingStrictly,
     indicesOfOccurence,
     leftJustifyWith,
     monotonic,
     multiplex,
     packets,
     pam,
     prefixes,
     quicksort,
     quicksortWith,
     reps,
     rightJustifyWith,
     splitFromRight,
     suffixes,
     takeFromRight,
     takeUntil,
     takeWhileFromRight,
     transpose)
     where
    
    group elements of sequence in blocks of given size
    Note: For any sequence xs, n::Int with n > 0,
    concat(blocks n xs) = xs
    
     blocks :: Int -> [a] -> [ [a] ]
     blocks blockSize =
     takeWhile(not . null) . map fst .
     iterate(splitAt blockSize . snd) . splitAt blockSize
    
    
    group elements of sequence in blocks of given size
    pad last group if necessary to make it the right length
    
     blocksRigid :: Int -> a -> [a] -> [ [a] ]
     blocksRigid blockSize pad =
     map(leftJustifyWith pad blockSize) . blocks blockSize Appendix — Some Useful Modules 138
    
    
    package sequence into subsequences terminated by delimiter;
    if xs is x1 ++ [d1] ++ x2 ++ [d2] ++ ... ++ xn ++ [dn] or
    if xs is x1 ++ [d1] ++ x2 ++ [d2] ++ ... ++ xn
    where each d satisfies (isDelimiter d),
    and no element e of any x-i satisifies
    (isDelimiter e)
    then (packets xs) is [x1, x2, ..., xn]
    
     packets :: (a -> Bool) -> [a] -> [[a]]
     packets isDelimiter =
     map fst . takeWhile(not . and . map null . pam[fst, snd]) .
     iterate(break isDelimiter . drop 1 . snd) . break isDelimiter
    
    
    multiplex a sequence of streams into one stream
    using round-robin alternation among streams with elements remaining
    Note: if s and t are different elements of the argument of multiplex
    and length(s) >= length(t), then the delivered sequence contains
    an element from s between each succesive element from t
    Example: multiplex["abc", "12345", "wxyz"] = "a1wb2xc3y4z5"
    
     multiplex :: [[a]] -> [a]
     multiplex = concat . foldr multiInsert [ ]
    
    
    insert elements of the first argument as initial elements of the
    sequences in the second argument
    
     multiInsert :: [a] -> [[a]] -> [[a]]
     multiInsert xs yss = matchingPairs ++ tailOfLongerOne
     where
     matchingPairs = zipWith (:) xs yss
     tailOfLongerOne = (map(:[ ]) . drop n) xs ++ drop n yss
     n = length matchingPairs
    
    
    prefixes delivers all of the non-empty prefixes of its argument:
    prefixes [x1, x2, x3, ...] = [[x1], [x1, x2], [x1, x2, x3], ... ]
    
     prefixes :: [a] -> [[a]]
     prefixes = drop 1 . scanl (++) [ ] . map(:[ ])
    
    
    suffixes delivers all of the non-empty suffixes of its argument:
    suffixes [x1, x2, x3, ...] = [[x1, x2, x3, ...],
    [x2, x3, ...],
    [x3, ...],
    ... ]
    
     suffixes :: [a] -> [[a]]
     suffixes = takeWhile(not . null) . iterate(drop 1)
    
    
    find indices in a sequence where an item occurs
    
     indicesOfOccurence :: Eq a => a -> [a] -> [Int]
     indicesOfOccurence item items = Appendix — Some Useful Modules 139
     foldr addIndex [] (zip items [0..])
     where
     addIndex (x,index) indexes
     | x == item = [index] ++ indexes
     | otherwise = indexes
    
    
    justify a sequence in a field of a given width
    (deliver original sequence if given field-width is too narrow)
    
     leftJustifyWith, rightJustifyWith, centerInField ::
     a -> Int -> [a] -> [a]
     leftJustifyWith pad fieldWidth xs =
     xs ++ reps (max 0 (fieldWidth - length xs)) pad
     rightJustifyWith pad fieldWidth xs =
     reps (max 0 (fieldWidth - length xs)) pad ++ xs
     centerInField pad width xs =
     reps leftPadLength pad ++ xs ++ reps rightPadLength pad
     where
     leftPadLength = max 0 ((width - lengthOfSequence) `div` 2)
     rightPadLength
     = max 0 (width - (leftPadLength + lengthOfSequence))
     lengthOfSequence = length xs
    
    
    form a sequence consisting of n copies of a given element
    
     reps :: Int -> a -> [a]
     reps n = take n . repeat
    
    
    shortest prefix of a sequence containing an element
    that satisfies a given predicate
    
     takeUntil :: (a -> Bool) -> [a] -> [a]
     takeUntil predicate xs = prePredicate ++ take 1 others
     where
     (prePredicate, others) = break predicate xs
    
    
    from-the-right versions of take, drop, and split
    
     takeFromRight, dropFromRight :: Int -> [a] -> [a]
     takeWhileFromRight, dropWhileFromRight ::
     (a -> Bool) -> [a] -> [a]
     splitFromRight :: Int -> [a] -> ([a], [a])
     takeFromRight n xs = drop (max 0 (length xs - n)) xs
     dropFromRight n xs = take (max 0 (length xs - n)) xs
     splitFromRight n xs = splitAt (max 0 (length xs - n)) xs
     takeWhileFromRight p = reverse . takeWhile p . reverse
     dropWhileFromRight p = reverse . dropWhile p . reverse
    
    
    concatenate, but include a standard element between appendees
    Note: if ws::[String], then concatWithSpacer " " ws = unwords ws
    
     concatWithSpacer :: [a] -> [[a]] -> [a]
     concatWithSpacer spacer [ ] = [ ]
     concatWithSpacer spacer nonEmptyList@(x : xs) = Appendix — Some Useful Modules 140
     foldr1 insertSpacer nonEmptyList
     where
     insertSpacer x1 x2 = x1 ++ spacer ++ x2
    
    
    apply a function to an argument
    
     apply :: (a -> b) -> a -> b
     apply f x = f x
    
    
    dual of map: apply sequence of functions to argument
    
     pam :: [a -> b] -> a -> [b]
     pam fs x = zipWith apply fs (repeat x)
    
    
    arrange sequence elements in increasing order
    
     quicksort :: Ord a => [a] -> [a]
     quicksort (firstx : xs) =
     quicksort[x | x <- xs, x < firstx] ++ [firstx] ++
     quicksort[x | x <- xs, not(x < firstx)]
     quicksort [ ] = [ ]
    
    
    arrange sequence elements in order according to given ordering
    
     quicksortWith :: (a -> a -> Bool) -> [a] -> [a]
     quicksortWith precedes (firstx : xs) =
     quicksortWith precedes [x | x <- xs, precedes x firstx] ++
     [firstx] ++
     quicksortWith precedes [x | x <- xs, not(precedes x firstx)]
     quicksortWith precedes [ ] = [ ]
    
    
    check to see if a sequence is monotonic wrt a given transitive relation
    
     monotonic :: (a -> a -> Bool) -> [a] -> Bool
     monotonic precedes xs = (and . zipWith precedes xs . drop 1) xs
    
    
    check to see if a sequence is increasing, decreasing, or flat
    
     allEqual :: Eq a => [a] -> Bool
     increasing, increasingStrictly,
     decreasing, decreasingStrictly :: Ord a => [a] -> Bool
     allEqual = monotonic(==)
     increasing = monotonic(<=)
     increasingStrictly = monotonic(<)
     decreasing = monotonic(>=)
     decreasingStrictly = monotonic(>)
    
    
    interchange rows and columns in a column of rows;
    the i-th element of the j-th sequence of the delivered result
    is the j-th element of the i-th sequence of the argument
    Note: successive rows may decrease in length;
    that is, transpose works properly on upper-triangular matrices Appendix — Some Useful Modules 141
    
     transpose :: [[a]] -> [[a]]
     transpose = foldr patchRowAcrossColumns [ ]
     where
     patchRowAcrossColumns row columns =
     zipWith (:) row (columns ++ repeat [ ])
    
    end of SequenceUtilities module
     module IOutilities
     (capitalizeWord,
     centerString,
     displayStringsInColumns,
     getCookedLine,
     integralFromString,
     interpretBackspaces,
     isEmptyLine,
     leftJustify,
     realFloatFromString,
     rightJustify,
     scientificFormatRealFloat,
     spaces,
     standardFormatRealFloat,
     trim,
     trimRight)
     where
    
     import SequenceUtilities
     (blocks, centerInField, concatWithSpacer,
     leftJustifyWith, rightJustifyWith,
     splitFromRight, reps, transpose)
    
     import Char(isSpace)
    
    convert string denoting Integral number to class Integral
    
     integralFromString :: (Integral i, Read i) => String -> i
     integralFromString intStringWithOptionalSign = x
     where
     [(x, _)] = reads intStringWithOptionalSign
    
    
    convert string denoting RealFloat number to class RealFloat
    
     realFloatFromString :: (RealFloat r, Read r) => String -> r
     realFloatFromString floatString = x
     where
     [(x, _)] = reads(fixedUpFloatString ++ exponentAndAfter)
     fixedUpFloatString
     | null beforeDecimalPt = sign ++ "0" ++ decimalPtAndAfter
     | null decimalPtAndAfter = sign ++ afterSign ++ ".0"
     | otherwise = floatString
     (beforeExponent, exponentAndAfter) = break atE floatString
     (beforeSign, signAndAfter) = break (== '-') beforeExponent
     (sign, afterSign)
     | null signAndAfter = ("", beforeSign)
     | otherwise = splitAt 1 signAndAfter
    Appendix — Some Useful Modules 142
     (beforeDecimalPt, decimalPtAndAfter) = break (== '.') afterSign
     (decimalPt, afterDecimalPt)
     | null decimalPtAndAfter = ("", beforeDecimalPt)
     | otherwise = splitAt 1 decimalPtAndAfter
     atE c = 'e' == toLower c
    
    
    deliver string denoting a RealFloat number in standard format
    
     standardFormatRealFloat :: RealFloat r => Int -> r -> String
     standardFormatRealFloat numberOfDecimalPlaces x =
     signPart ++ integerPart ++ "." ++ fractionPart
     where
     xAsString = (stripParentheses . show) x
     (signPart, significand, exponent) =
     componentsOfRealFloatString xAsString
     shiftDistAndDirection = exponent + numberOfDecimalPlaces + 1
     roundedSignificand
     | significand == 0 = 0
     | shift >= 0 = (significand*shift + 5) `div` 10
     | shift < 0 = (significand `div` shift + 5) `div` 10
     shift = 10^(abs shiftDistAndDirection)
     roundedSignificandAsString =
     (leftJustifyWith '0' numberOfDecimalPlaces . show)
     roundedSignificand
     (integerPart, fractionPart) =
     splitFromRight numberOfDecimalPlaces roundedSignificandAsString
    
    
    deliver string denoting a RealFloat number in scientific notation
    
     scientificFormatRealFloat :: RealFloat r => Int -> r -> String
     scientificFormatRealFloat numberOfSignificantDigits x =
     signPart ++ integerPart ++ "." ++ fractionPart ++
     "E" ++ exponentSign ++ exponentPart
     where
     xAsString = (stripParentheses . show) x
     (signPart, significand, exponent) =
     componentsOfRealFloatString xAsString
     numberOfDigitsInSignificand = length(show significand)
     shift = numberOfSignificantDigits + 1 - numberOfDigitsInSignificand
     roundedSignificand
     | significand == 0 = 0
     | shift >= 0 = (significand * 10^shift + 5) `div` 10
     | shift < 0 = (significand `div` 10^(-shift) + 5) `div` 10
     shiftedExponent
     | roundedSignificand == 0 = 0
     | otherwise = exponent - shift + numberOfSignificantDigits
     exponentPart = rightJustifyWith '0' 2
     ((stripParentheses . show . abs) shiftedExponent)
     exponentSign
     | shiftedExponent >= 0 = "+"
     | shiftedExponent < 0 = "-"
     roundedSignificandAsString =
     (leftJustifyWith '0' numberOfSignificantDigits . show)
     roundedSignificand
     (integerPart, fractionPart) =
     splitAt 1 roundedSignificandAsString
    Appendix — Some Useful Modules 143
    
    break string denoting RealFloat number into sign/significand/exponent
    
     componentsOfRealFloatString :: String -> (String, Integer, Int)
     componentsOfRealFloatString realFloatString =
     (signAsString, significand, decimalPointPosition)
     where
     (signAsString, unsignedPart) =
     span (`elem` ['-', ' ']) realFloatString
     (integerPartAsString, fractionPlusExponent) =
     span isDigit unsignedPart
     (fractionPartWithDecimalPointMaybe, exponentPartWithE) =
     break (`elem` ['e', 'E']) fractionPlusExponent
     (decimalPoint, fractionPartAsString) =
     span (== '.') fractionPartWithDecimalPointMaybe
     (ePart, exponentAsStringWithSignMaybe) =
     span (`elem` ['e', 'E']) exponentPartWithE
     exponentAsString = dropWhile (== '+') exponentAsStringWithSignMaybe
     exponent
     | null exponentAsString = 0
     | otherwise = integralFromString exponentAsString
     significandAsString = integerPartAsString ++ fractionPartAsString
     significand = toInteger(integralFromString significandAsString)
     decimalPointPosition = exponent - length fractionPartAsString
    
    
    remove all parentheses from string
    
     stripParentheses :: String -> String
     stripParentheses = filter(/= '(') . filter (/= ')')
    
    
    justify string within field of given width
    
     leftJustify, centerString, rightJustify ::
     Int -> String -> String
     rightJustify = rightJustifyWith ' '
     leftJustify = leftJustifyWith ' '
     centerString = centerInField ' '
    
    
    string comprising a given number of spaces
    
     spaces :: Int -> String
     spaces numberOfSpaces = reps numberOfSpaces ' '
    
    
    capitalize first character in string, make rest lower case
    
     capitalizeWord :: String -> String
     capitalizeWord w
     | null w = ""
     | otherwise = [toUpper firstLetter] ++ map toLower others
     where
     ([firstLetter], others) = splitAt 1 w
    
    
    deliver string that will display a sequence of strings as a sequence
    of pages, with strings appearing in sequence down successive columns Appendix — Some Useful Modules 144
    
     displayStringsInColumns ::
     Int -> Int -> Int -> Int -> Int -> [String] -> String
     displayStringsInColumns pageWidth gapBetweenPages stringsPerColumn
     columnWidth gapBetweenColumns =
     concatWithSpacer(reps gapBetweenPages '\n') .
     map displayPage . map transpose . map(blocks stringsPerColumn) .
     blocks stringsPerPage . map(leftJustify columnWidth)
     where
     numberOfColumns = (pageWidth + gapBetweenColumns) `div`
     (columnWidth + gapBetweenColumns)
     stringsPerPage = stringsPerColumn * numberOfColumns
     displayPage =
     unlines . map(concatWithSpacer(spaces gapBetweenColumns))
    
    
    remove leading and trailing whitespace from a string
    
     trim :: String -> String
     trim = trimLeft . trimRight
    
    
    remove leading whitespace from a string
    
     trimLeft :: String -> String
     trimLeft = dropWhile isSpace
    
    
    remove trailing whitespace from a string
    
     trimRight :: String -> String
     trimRight = reverse . dropWhile isSpace . reverse
    
    
    deliver True iff argument contains no characters other than blanks
    
     isEmptyLine :: String -> Bool
     isEmptyLine = (=="") . dropWhile(==' ')
    
    
    retrieve line from keyboard and interpret backspaces
    
     getCookedLine :: IO(String)
     getCookedLine =
     do
     rawLine <- getLine
     return(interpretBackspaces rawLine)
    
    
    interpret backspaces in string, delivering string free of BS
    
     interpretBackspaces :: String -> String
     interpretBackspaces =
     reverse . foldl interpretIfBS [ ]
    
     interpretIfBS :: String -> Char -> String
     interpretIfBS [ ] '\b' = [ ]
     interpretIfBS (c : cs) '\b' = cs
     interpretIfBS cs c = [c] ++ cs 2 Appendix — Some Useful Modules 145
     module NumericUtilities
     (average,
     correlation,
     digitize,
     standardDeviation,
     standardDeviationUnbiased,
     nudge)
     where
     import VectorOperations(innerProduct, norm)
    
    n-way analog-to-digital converter for a <= x < b
    
     digitize:: RealFrac num => Int -> num -> num -> num -> Int
     digitize n a b x
     | xDist < halfstep = 0-- avoid boundary glitches
     | xDist > lastHalfstep = n
     | otherwise = floor(xDist/dx)
     where
     xDist = x - a
     dx = span/(fromIntegral n)
     halfstep = dx/2
     lastHalfstep = span - halfstep
     span = b - a
    
    increase magnitude by an almost negligible amount
    
     nudge :: RealFrac num => num -> num
     nudge x = (last . takeWhile(/= x) . take 100 .
     map (x+) . iterate (/2)) x
    
    arithmetic mean
    
     average :: Fractional num => [num] -> num
     average = foldl includeAnotherSample 0 . zip[1 ..]
     where
     includeAnotherSample runningAverage (sampleNumber, sample) =
     runningAverage + (sample - runningAverage)/fromRealFrac sampleNumber
    
    standard deviation
    
     standardDeviation :: Floating num => [num] -> num
     standardDeviation samples =
     (sqrt . average . map deviationSquared) samples
     where
     mu = average samples
     deviationSquared x = abs(x - mu)^2
    
    standard deviation - unbiased estimate
    
     standardDeviationUnbiased :: Floating num => [num] -> num
     standardDeviationUnbiased samples =
     (standardDeviation samples)*((fromIntegral n)/fromIntegral(n - 1))
     where
     n = length samples
    
    correlation
    
     correlation :: RealFloat num => [num] -> [num] -> num
     correlation xs ys = innerProduct xs ys / (norm xs * norm ys)

