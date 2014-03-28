{--
transposes lines in input file
e.g. hello world
     how are
becomes
     hh
     eo
     lw
     l 
      r
     we
--}
        
import System.Environment (getArgs)

-- e.g. zipn "abc" "def" -> ["ad","be","cf"]
zipn :: String -> String -> [String]
zipn _ [] = []
zipn [] _ = []
zipn (x:xs) (y:ys) = [[x] ++ [y]] ++ zipn xs ys

-- e.g. func ["ad","be","cf"] "gh" -> ["adg","beh"]
func :: [String] -> String -> [String]
func _ [] = []
func [] _ = []
func (x:xs) (y:ys) = [x ++ [y]] ++ (func xs ys)

continuePrint :: [String] -> [String] -> [String]
continuePrint x [] = x
continuePrint x y = continuePrint (func x (head y)) (tail y)

startPrint :: [String] -> [String]
startPrint (x:y:z) = continuePrint (zipn x y) z
startPrint [x] = [x] -- file has only one line
startPrint [] = [] -- file has no lines

loadFile x = putStrLn (unlines (startPrint (lines x)))

-- phil5's solution (http://book.realworldhaskell.org/admin/comments/comment/16593/). elegant!!
myTranspose input = putStrLn (unlines (recjoin (lines input)))
recjoin xs
           | null xs = []
           | any null xs = []
           | otherwise = (map head xs) : recjoin (map tail xs)


-- framework
interactWith function inFile = do
  input <- readFile inFile
  function input
main = mainWith myFunction
       where mainWith function = do
               args <- getArgs
               case args of
                 [inFile] -> interactWith function inFile
                 _        -> error "One argument needed."
             myFunction = myTranspose
