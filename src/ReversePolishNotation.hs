{-
- Processing reverse polish notation using Haskell
-}
processRPN :: (Num a, Read a, Fractional a) => String -> [a]
processRPN = foldl f [] . words

f :: (Fractional a, Read a, Num a) => [a] -> String -> [a]
f [] x = [read x]
f [a] x = case x of
                    "+" -> error "Incorrect expression given"
                    "-" -> error "Incorrect expression given"
                    "*" -> error "Incorrect expression given"
                    "/" -> error "Incorrect expression given"
                    _   -> (read x):[a]
f m@(a:b:ab') x = case x of
                    "+" -> (b+a):ab'
                    "-" -> (b-a):ab'
                    "*" -> (b*a):ab'
                    "/" -> (b/a):ab'
                    _   -> (read x):m

main :: IO ()
main = do
         putStrLn "Enter an expression in RPN notation:"
         input <- getLine
         putStrLn $ (show . head . processRPN) input
