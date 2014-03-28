-- prints first word of every line in input file
import System.Environment (getArgs)

-- version 1 (naive)
getFirstWords [] = []
getFirstWords x = let (prefix, suffix) = break isLineTerminator x
                                         where isLineTerminator y = (y == '\r') || (y == '\n')
                      processSuffix x = case x of
                                        ('\r':'\n':rest) -> getFirstWords rest
                                        ('\n':rest)      -> getFirstWords rest
                                        ('\r':rest)      -> getFirstWords rest
                                        _                -> []
                  in
                  case (words prefix) of
                     (x:_) -> [x] ++ (processSuffix suffix)
                     _     -> []

printWords x = putStr (unlines (getFirstWords x))

-- version 2
getWords' x = map func (lines x)
                where func x = [unwords (take 1 (words x))] -- unwords preserves any empty lines in the input file by adding a space to it because
                                                            -- concat deletes empty lists.
                                                            -- we can't use head instead of take 1 coz head fails on empty list (and hence for any
                                                            -- empty lines in the input file
             
printWords' x = putStr (unlines (concat (getWords' x)))

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
             myFunction = printWords'