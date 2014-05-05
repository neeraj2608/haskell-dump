{-
- Simple ray tracing
-}
module RayTracer where

import Control.Applicative
import Vector

--------------------------------------------------------------------------------
writePPM :: Image -> FilePath -> IO ()
writePPM im file = do
    let colors = getPrintableColorData $ colordata im
    let s = "P3\n" ++
            (show . width $ im) ++
            "\n" ++
            (show . height $ im) ++
            "\n" ++
            "255" ++
            "\n" ++
            colors
    writeFile file s
    where
        getPrintableColorData :: [Color] -> String
        getPrintableColorData x = unlines $ map unwords $ map show <$> x

--------------------------------------------------------------------------------
traceScene :: Scene -> Image -> Image
traceScene s i = pickUppermostSurface i $ map (`getSurfaceRayIntersections` i) s
    where
        -- we have [Image] with ray intersections for each surface in the
        -- scene. We assume opaque surfaces so only the uppermost surface
        -- will show
        pickUppermostSurface :: Image -> [[(Maybe Double, Color)]] -> Image
        pickUppermostSurface im y = updateImage im $ map snd [f y index | index <- [0..(length . head) y - 1]]

        f :: [[(Maybe Double, Color)]] -> Int -> (Maybe Double, Color)
        f y index = foldr (g index) (Nothing, white) y

        g :: Int -> [(Maybe Double, Color)] -> (Maybe Double, Color) -> (Maybe Double, Color)
        g index y acc | fst curr > fst acc = curr
                      | otherwise = acc
                      where curr = y !! index

        updateImage :: Image -> [Color] -> Image
        updateImage x y = x {colordata = y}

        white = [255, 255, 255]

getSurfaceRayIntersections :: Surface -> Image -> [(Maybe Double, Color)]
getSurfaceRayIntersections s i = zip f (repeat $ color s)
    where
        f :: [Maybe Double]
        f = map ((`intersect` s) . primaryRayForPixel) [ (x,y) | x <- [0..width i - 1], y <- [0..height i - 1]]
