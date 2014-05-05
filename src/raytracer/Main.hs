module Main where

import RayTracer
import Types
--------------------------------------------------------------------------------
main :: IO ()
--main = writePPM (Image 50 50 $ take 2500 $ cycle [[x,x,x] | x <- [0..255]]) "test.ppm"
main = do
    let im = traceScene testScene $ Image 600 600 []
    writePPM im "test.ppm"

testScene :: [Surface]
testScene = [Sphere [500.0, 300.0, 0.0] 100.0 [50,70,90],
             Sphere [450.0, 200.0, 80.0] 80.0 [250,30,10],
             Sphere [350.0, 400.0, -50.0] 120.0 [200,220,250]]
