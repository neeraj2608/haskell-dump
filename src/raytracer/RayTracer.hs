{-
- Simple ray tracing
-}
module RayTracer where

import Control.Applicative
import Types
import Control.Arrow ((&&&))

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
traceScene s image = updateImage image $ pickUppermostSurface $ getSurfaceRayIntersections [ (x, y, s) | y <- [0..h - 1], x <- [0..w - 1] ]
    where
        w = width image
        h = height image

        pickUppermostSurface :: [[(Color, Maybe Double)]] -> [Color]
        pickUppermostSurface = map (fst . fold)

        fold :: [(Color, Maybe Double)] -> (Color, Maybe Double)
        fold = foldr f (white, Nothing)

        f :: (Color, Maybe Double) -> (Color, Maybe Double) -> (Color, Maybe Double)
        f x y | snd x > snd y = x
              | otherwise = y

        updateImage :: Image -> [Color] -> Image
        updateImage x y = x {colordata = y}

        white = [255, 255, 255]

getSurfaceRayIntersections :: [(Int, Int, Scene)] -> [[(Color, Maybe Double)]]
getSurfaceRayIntersections = map f
    where
        f :: (Int, Int, Scene) -> [(Color, Maybe Double)]
        f (x, y, z) = map (color Control.Arrow.&&& intersect (primaryRayForPixel (x,y)) ) z
