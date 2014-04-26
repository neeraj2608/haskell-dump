{-
- Minimax algorithm
- Based on http://page.mi.fu-berlin.de/oezbek/brueckenkurs/HaskellQuestions.pdf, page 124Q
- Author: Raj
-}
module MiniMax(
               Player(..),
               perfectGameFromPosition)
               where

data Player = Player1 | Player2

data Game position = Plays position [Game position]

-- perfectGameFromPosition gives the final configuration of the game if
-- played perfectly from position p onwards
-- onemoveaway is a function that returns all next board configurations attainable from the
-- current one
perfectGameFromPosition :: Real num => (position -> [position]) -> (position -> num) -> (position -> Player) -> position -> position
perfectGameFromPosition onemoveaway score chooseplayer p = play (chooseplayer p) score (gameTree onemoveaway p)

-- generate game tree
gameTree :: (position -> [position]) -> position -> Game position
gameTree onemoveaway p = Plays p (map (gameTree onemoveaway) (onemoveaway p))

play :: Real num => Player -> (position -> num) -> Game position -> position
--  Player1 wants to maximize her score
play Player1 score (Plays p games) | null games = p
                                   | otherwise = foldr1 (maxPosition score)
                                                        (map (play Player2 score) games)
--  Player2 wants to minimize her score
play Player2 score (Plays p games) | null games = p
                                   | otherwise = foldr1 (minPosition score)
                                                        (map (play Player1 score) games)

maxPosition :: Real num => (position -> num) -> position -> position -> position
maxPosition score x y | score x > score y = x
                      | otherwise = y

minPosition :: Real num => (position -> num) -> position -> position -> position
minPosition score x y | score x < score y = x
                      | otherwise = y
