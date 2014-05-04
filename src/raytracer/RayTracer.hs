module RayTracer where

import Control.Applicative

type Color = [Int]
data Image = Image {width :: Int,
                 height :: Int,
                 colordata :: [Color]}

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

main :: IO ()
main = writePPM (Image 50 50 $ take 2500 $ cycle [[x,x,x] | x <- [0..255]]) "test.ppm"
