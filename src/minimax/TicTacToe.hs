{-
- Uses the MiniMax algorithm to play a game of tictactoe
- Based on http://page.mi.fu-berlin.de/oezbek/brueckenkurs/HaskellQuestions.pdf, page 124Q
- Author: Raj
-}
module TicTacToe where

import MiniMax (Player(..), perfectGameFromPosition)
import Data.List (intersperse)

-- the board is represented by an [Int] of length 9
-- [x, x, x,
--  x, x, x,
--  x, x, x]
-- even numbers represent moves by player2; odd by player 1
-- 0s represent unoccupied positions
data TPosition = Grid [Int]
                 deriving Show

-- if the max marker is even, Player2 just played
-- else, Player1 just played
choosePlayer :: TPosition -> Player
choosePlayer (Grid pos) | (even . maximum) pos = Player1
                      | otherwise = Player2

scoreGame :: Real num => TPosition -> num
scoreGame board | Player1 `didWin` board = 2 -- remember that player1 is looking for a larger score. Hence, her win must be higher than player 2's
                | Player2 `didWin` board = 1
                | otherwise = 0 -- need to play some more

didWin :: Player -> TPosition -> Bool
didWin Player1 board = checkWins odd board
didWin Player2 board = checkWins even board

checkWins :: (Int -> Bool) -> TPosition -> Bool
checkWins evenOrOdd (Grid board) | any (==True) $ map (all evenOrOdd) (map (map (board!!)) winCoords) = True
                                 -- explanation: for each list in winCoords,
                                 -- map the corresponding grid elements.
                                 -- Then, check if that grid element is even
                                 -- or odd (depends on player: for Player1,
                                 -- we look for odd; for Player2 even. If
                                 -- any one of those lists have all odd
                                 -- (even), that combination is a winning
                                 -- position for Player1 (Player2) and we
                                 -- can declare that game to be won by that
                                 -- player.
                                 | otherwise = False
                                 where winCoords = [[0,3,6],[1,4,7],[2,5,8],[0,1,2],[3,4,5],[6,7,8],[0,4,8],[2,4,6]]
                                       -- winCoords corresponds to winning
                                       -- positions (all rows, all columns or
                                       -- diagonals)

-- returns all valid positions one position away from the current one
-- every one of these positions will be examined by perfectGameFromPosition
-- and the best one will be picked
oneMoveAway :: TPosition -> [TPosition]
oneMoveAway (Grid board) = filter (\(Grid x) -> x /= []) $ map copyBoardWithIncrement (zip board [0..])
                           where -- copyBoardWithIncrement replaces a 0
                                 -- (unoccupied) position on the board with
                                 -- 1+(maximum position on board). It
                                 -- creates separate copies of the board
                                 -- for every 0 in the board
                                 -- e.g. [1,0,2,0] yields [1,3,2,0] and
                                 -- [1,0,2,3]
                                 copyBoardWithIncrement :: (Int,Int) -> TPosition
                                 copyBoardWithIncrement (0, i) = Grid $ replace 0 i board $ (maximum board)+1
                                 copyBoardWithIncrement _ = Grid []
                                 replace _ _ [] _ = []
                                 replace i j (x:xs) newval | (i==j) = newval : (replace (i+1) j xs newval)
                                                           | otherwise = x : (replace (i+1) j xs newval)

displayBoardAndPlay :: TPosition -> IO ()
displayBoardAndPlay game@(Grid board) = do
                                          if(all (==0) board)
                                          then
                                            putStrLn "Game needs at least one move!"
                                          else do
                                            printGrid game
                                            putStrLn "Playing game, this may take a while...\n"
                                            printGrid $ (perfectGameFromPosition oneMoveAway scoreGame choosePlayer game)
                                            putStrLn "Done!"

printGrid :: TPosition -> IO ()
printGrid (Grid board) = putStrLn $ addnewlines $ intersperse ' ' $ map (printgrid) board

addnewlines :: String -> String
addnewlines [] = []
addnewlines x = (take 6 x) ++ "\n" ++ addnewlines (drop 6 x)

printgrid :: (Int -> Char)
printgrid x | x==0 = '_'
            | even x = '#'
            | otherwise = 'X'

main :: IO ()
main = displayBoardAndPlay (Grid [0,0,0,
                                  0,0,0,
                                  0,0,0])
