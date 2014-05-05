module Main where

import RayTracer
import Vector
--------------------------------------------------------------------------------
main :: IO ()
--main = writePPM (Image 50 50 $ take 2500 $ cycle [[x,x,x] | x <- [0..255]]) "test.ppm"
main = do
    let im = traceScene test_scene $ Image 60 60 []
    writePPM im "test.ppm"

test_scene :: [Surface]
test_scene = [Sphere [0.0, 0.0, 0.0] 10.0 [50,70,90],
              Sphere [45.0, 100.0, 80.0] 50.0 [250,30,10],
              Sphere [15.0, -100.9, -50.0] 120.0 [200,220,250]]
