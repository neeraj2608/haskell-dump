-- extracts defines from C headers
import System.Environment (getArgs)
import Data.List (isPrefixOf)

extractDefines = putStr . unlines . map (head . tail . words) . filter ("#define" `isPrefixOf`) . lines

-- framework
interactWith function inFile = do
  input <- readFile inFile
  function input
main = mainWith myFunction
       where mainWith function = do
               args <- getArgs
               case args of
                 [inFile] -> interactWith function inFile
                 _        -> error "Input file name is required."
             myFunction = extractDefines