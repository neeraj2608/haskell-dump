{-# LANGUAGE FlexibleInstances #-}

module Types where

import Debug.Trace

--------------------------------------------------------------------------------
type Color = [Int]

data Image = Image {width :: Int,
                    height :: Int,
                    colordata :: [Color]}

type Vector = [Double]

normalize :: Vector -> Vector
normalize x = map (flip (/) (f x)) x
    where f = sqrt . sum . sqr

instance Num Vector where
    (*) = zipWith (*)
    (-) = zipWith (-)
    (+) = zipWith (+)
    fromInteger = take 3 . repeat . fromIntegral

dotProduct :: Vector -> Vector -> Double
dotProduct x y = sum $ x * y

data Surface = Sphere {center :: Vector, radius :: Double, color :: Color}

instance Show Surface where
    show x = "c: "++ show (center x) ++", r: "++ show (radius x)

-- we assume for our intersection calculations that our
-- direction vector is a unit vector
data Ray = Ray {origin :: Vector, direction :: Vector}

-- a scene consists of multiple surfaces and light sources
type Scene = ([Surface],[Vector])

type Pixel = (Int, Int)

--------------------------------------------------------------------------------
intersect :: Ray -> Surface -> Maybe Double
intersect ray surface = f --(trace (show f) f)
    where
        f :: Maybe Double
        f = if d < 0
                then Nothing
                else Just $ (+ (-1000)) $ min (-b + (sqrt d)) (-b - (sqrt d))
        d  = (sqr b) - ((v `dotProduct` v) - sqr r)
        b = v `dotProduct` (direction ray)
        r = radius surface
        v = (origin ray) - (center surface)

sqr :: Num a => a -> a
sqr x = x * x
