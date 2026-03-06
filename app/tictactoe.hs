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

main = do
    print (calMark X 10)