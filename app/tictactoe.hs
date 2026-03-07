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

-- Player
-- used to represent Player1, Player2, or Computer
data Player = P1 | P2 | COM deriving (Show)
-- PlayerList type
-- a tuple of (Player, Player), used to represent the current active player(s)
-- e.g. (P1, P2), (P1, COM)
type PlayerList = (Player, Player)

-- calculate the winner based on the total mark
-- given a PlayerList (x, y) to represent the active player
-- if mark is >= 3 (three consecutive O in row / column / diagonal), then winner is x
-- if mark is <= -3 (three consecutive -1), then winner is y
-- otherwise, there is no winner
calWinner :: PlayerList -> Int -> Maybe Player
calWinner (x,y) mark
    | mark >= 3 = Just x
    | mark <= -3 = Just y
    | otherwise = Nothing

main = do
    print (calWinner (P1, COM) (-3))
    print (calWinner (P1, COM) 0)
    print (calWinner (P1, COM) 3)
