import Prelude

-- Symbol
-- used to represent the 'O' or 'X' on the board
-- 'Empty' represent the grid has not been ticked yet
data Symbol = O | X | Empty deriving (Show, Eq)

-- calculate the mark of the game (the row / column / diagonal)
-- O represent 1; X represent -1
calMark :: Symbol -> Int -> Int
calMark x y
    | x == O = 1 + y
    | x == X = -1 + y
    | otherwise = y

-- the Playboard type
-- used to represent the 3x3 playboard filled with Symbol
-- (a 2D array / matrix)
type Playboard = [[Symbol]]

-- playboard value
playboard :: Playboard
-- generate matrix by list comprehension
-- for i in [0..2], you generate a row
-- to generate each row, it is for j in [0..2] to generate each column, and that each column is Empty Symbol
playboard = [[Empty | column <- [0..2]] | row <- [0..2]]

main = do
    print (playboard)