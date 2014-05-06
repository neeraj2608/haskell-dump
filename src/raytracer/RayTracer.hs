{-
- Simple ray tracing
-}
module RayTracer where

import Control.Applicative
import Types

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
traceScene s image = updateImage image $ pickUppermostSurface $
    getSurfaceRayIntersections [ (x, y, fst s) | y <- [0..h - 1], x <- [0..w - 1] ] $ snd s
    where
        w = width image
        h = height image

        pickUppermostSurface :: [[(Color, Maybe Double)]] -> [Color]
        pickUppermostSurface = map (fst . fold)

        fold :: [(Color, Maybe Double)] -> (Color, Maybe Double)
        fold = foldr f (white, Nothing)

        f :: (Color, Maybe Double) -> (Color, Maybe Double) -> (Color, Maybe Double)
        f (_, Nothing) (y, Just z) = (y, Just z)
        f (x, Just z) (_, Nothing) = (x, Just z)
        f (_, Nothing) (_, Nothing) = (white, Nothing)
        f x y | snd x < snd y = x
              | otherwise = y

        updateImage :: Image -> [Color] -> Image
        updateImage x y = x {colordata = y}

white = [255, 255, 255]

--------------------------------------------------------------------------------
getSurfaceRayIntersections :: [(Int, Int, [Surface])] -> [Vector] -> [[(Color, Maybe Double)]]
getSurfaceRayIntersections a lights = map f a
    where
        f :: (Int, Int, [Surface]) -> [(Color, Maybe Double)]
        f (x, y, z) = map (g x y) z

        g :: Int -> Int -> Surface -> (Color, Maybe Double)
        g x y surf = (clr, iSectPoint)
          where
              clr = case iSectPoint of
                        Nothing -> color surf
                        Just z -> deduceColor surf [fromIntegral x, fromIntegral y, z] lights
              iSectPoint = primaryRayForPixel (x,y) `intersect` surf

        deduceColor :: Surface -> Vector -> [Vector] -> Color
        deduceColor surf iSectPoint = foldr f (color surf)
            where
                f :: Vector -> Color -> Color
                f light c | shade < 0 = map (round . (\x -> fromIntegral x * ambientCoeff)) c
                          | otherwise  = map (round . (\x -> fromIntegral x * (ambientCoeff + (diffCoeff * shade)))) c
                    where
                        shade = normalize (iSectPoint - (center surf)) `dotProduct` normalize (light - iSectPoint)
                        ambientCoeff = 0.2
                        diffCoeff = 0.8

--------------------------------------------------------------------------------
-- Use orthographic projection
primaryRayForPixel :: Pixel -> Ray
primaryRayForPixel (x,y) = Ray [fromIntegral x,fromIntegral y,-1000] [0,0,1]
