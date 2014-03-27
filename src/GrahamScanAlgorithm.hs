{--
Graham's Scan Algorithm for finding convex hulls
--}
module GrahamScanAlgorithm where

import Data.List -- for sortBy

-- direction of turn
data Direction = Left | Right | Straight
                 deriving (Show, Eq)
turnDirection (x1,y1) (x2,y2) (x3,y3) | crossProduct == 0 = GrahamScanAlgorithm.Straight
                                      | crossProduct > 0 = GrahamScanAlgorithm.Left
                                      | crossProduct < 0 = GrahamScanAlgorithm.Right
                                      where crossProduct = (x2 - x1)*(y3 - y1) - (y2 - y1)*(x3 - x1)

-- directions of turns for each triplet
turnDirections x | length x < 3 = []
turnDirections ((x1,y1):(x2,y2):(x3,y3):rest) = [(turnDirection (x1,y1) (x2,y2) (x3,y3))] ++ turnDirections ((x2,y2):(x3,y3):rest)

-- Graham's scan algorithm for finding convex hulls

-- STEP 1: find the point with the lowest y coordinate
-- O(n)
findLowestY :: (Ord i, Num i) => [(i,i)] -> (i,i)
findLowestY [] = error "list too short"
findLowestY [(x,y)] = (x,y)
findLowestY ((x,y):(x1,y1):rest) | y == y1 
                                    = if(x < x1) then findLowestY ((x,y):rest) -- if y coords are equal, lower x wins
                                      else findLowestY ((x1,y1):rest)
                                 | y < y1 = findLowestY ((x,y):rest)
                                 | otherwise = findLowestY ((x1,y1):rest)

-- STEP 2: sort the points in the order of the angle that the line segment between each point and
-- the lowest point (we get it by calling findLowestY) makes with the x-axis
-- O(n log n)
sortPoints [] = []
sortPoints [(x,y)] = [(x,y)]
sortPoints x = sortBy sortPoints x
               where (xLow, yLow) = findLowestY x
                     sortPoints (x1,y1) (x2,y2) = compare p1Slope p2Slope
                                               where p1Slope = (y1-yLow)/(x1-xLow)
                                                     p2Slope = (y2-yLow)/(x2-xLow)

-- STEP 3: for the sorted list from Step 2,
-- if a triplet is a left turn, proceed to the next triplet
-- if a triplet is a right turn, discard the middle element of the triplet
--                               and continue with the next point
-- e.g. for sequence (a,b,c,d) where a, b, c, and d are each pairs of coordinates
-- if a,b,c is a left turn -> look at b,c,d
-- if a,b,c is a right turn -> look at a,c,d
startGraham [] = []
startGraham x | length x < 3 = x
startGraham ((x1,y1):(x2,y2):(x3,y3):rest) | ((turnDirection (x1,y1) (x2,y2) (x3,y3)) == GrahamScanAlgorithm.Right) = startGraham ((x1,y1):(x3,y3):rest)
                                           | otherwise = [(x1,y1)] ++ startGraham ((x2,y2):(x3,y3):rest)
                                           
testPath = startGraham [(0,0),(1,1),(1,2),(-1,2),(0,1.5),(-2,1.5),(-1,0)] -- should be [(0.0,0.0),(1.0,1.0),(1.0,2.0),(-1.0,2.0),(-2.0,1.5),(-1.0,0.0)]
