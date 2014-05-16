module Conc where

import Control.Concurrent
import ThreadManager

-- to see the difference without a managed thread that we can wait for,
-- compare main with test. In both these functions, we do the same thing
-- (putStrLn $ concat $ replicate 500 "hi")
-- In the main case, however, the done is always printed after all the "hi"s
-- In the test case, the done is interspersed with the "hi"s
main :: IO ()
main = do
        tMgr <- newManager
        tid <- forkManagedThread tMgr (do putStrLn $ concat $ replicate 500 "hi"; return ())
        waitFor tMgr tid
        print "done"
        return ()
  
test :: IO ()
test = do
        forkIO (do putStrLn $ concat $ replicate 500 "hi"; return ())
        print "done"
        return ()
