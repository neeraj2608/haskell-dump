import System.Environment (getArgs)

portableSplit :: String -> [String]

portableSplit [] = []
portableSplit x = 
  let (prefix, suffix) = break isLineTerminator x
                         where isLineTerminator y = (y == '\r') || (y == '\n')
  in prefix : case suffix of
                ('\r':'\n':rest) -> portableSplit rest
                ('\r':rest)      -> portableSplit rest
                ('\n':rest)      -> portableSplit rest
                _                -> []

fixLines x = unlines (portableSplit x)

interactWith function inFile outFile = do
  input <- readFile inFile
  writeFile outFile (function input)
  
main = mainWith myFunction
       where mainWith function = do
               args <- getArgs
               case args of
                 [inFile, outFile] -> interactWith function inFile outFile
                 _                 -> error "Two arguments needed."
             myFunction = fixLines -- myFunction :: String -> String