import Prelude

-- Symbol
-- used to represent the 'O' or 'X' on the board
-- 'Empty' represent the grid has not been ticked yet
data Symbol = O | X | Empty deriving (Show, Eq)
-----------------------------------------------------------------------------------------


-- the Playboard type
-- used to represent the 3x3 playboard filled with Symbol
-- (a 2D array / matrix)
type Playboard = [[Symbol]]

-- generate a new playboard (newPlayboard is value of an empty 3x3 grid that future playboard can copy from)
newPlayboard :: Playboard
-- generate matrix by list comprehension
-- for i in [0..2], you generate a row
-- to generate each row, it is for j in [0..2] to generate each column, and that each column is Empty Symbol
newPlayboard = [[Empty | column <- [0..2]] | row <- [0..2]]

-- the Position type
-- used to represent the position of the grid within playboard
type Position = (Row, Column)
type Row = Int
type Column = Int
-----------------------------------------------------------------------------------------


-- Player
-- used to represent Player1, Player2, or Computer
data Player = P1 | P2 | COM deriving (Show)
-- PlayerList type
-- a tuple of (Player, Player), used to represent the current active player(s)
-- e.g. (P1, P2), (P1, COM)
type PlayerList = (Player, Player)
-----------------------------------------------------------------------------------------

-- the Turn type
-- used to represent what is the current turn (how many Symbol has been placed, and the number of current Symbol)
type Turn = Int

-- the GameStatus type
-- used to represent a game (contain what is the current player list, the playboard, and the game turn)
type GameStatus = (PlayerList, Playboard, Turn)

-- the LegalMove type
-- used to determine if the player move is legal
type LegalMove = Bool

-- the TurnStatus type
-- used to determine if the previous turn is legal (check if player try to mark an occupied grid)
type TurnStatus = (LegalMove, GameStatus)
-----------------------------------------------------------------------------------------


-- calculate the mark of the game (the row / column / diagonal)
-- O represent 1; X represent -1
calMark :: Symbol -> Int -> Int
calMark x y
    | x == O = 1 + y
    | x == X = -1 + y
    | otherwise = y

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

-- get the current turn's Symbol based on the turn number
-- odd turn implies O is marking
-- even turn implies X is marking
-- O X can be controlled by either player or COM, depends on the playerlist
-- e.g. (P1, COM)   -> player is O, COMP is X
--      (COM, P1)   -> COM is O, player is X
getCurrentTurnSymbol :: Turn -> Symbol
getCurrentTurnSymbol turn
    | turn <= 1 = O
    | otherwise = if mod turn 2 == 1 then O else X

-- used to check if the current move is legal
-- if the current row and column is not Empty (it is marked with O or X), then the move is illegal
checkLegalMove :: Playboard -> Position -> LegalMove
checkLegalMove playboard (row, column)
    = if playboard !! row !! column == Empty then True else False

-- mark a grid of the playboard as "taken" (being marked by O or X)
-- take two arguments: GameStatus, Position
-- return: TurnStatus
-- 1. take GameStatus (to know the player list, playboard status, and current game turn)
-- 2. take Position (to know which grid to be marked)
-- 3. return TurnStatus (state if the previous move is legal and what is the outcome of the GameStatus)
markGrid :: GameStatus -> Position -> TurnStatus
markGrid (playerlist, playboard, turn) position
    = if not (checkLegalMove playboard position)
        then (False, (playerlist, playboard, turn))
        else (True, (playerlist, playboard, turn + 1))
-----------------------------------------------------------------------------------------


main = do
    print (calWinner (P1, COM) (-3))
    print (calWinner (P1, COM) 0)
    print (calWinner (P1, COM) 3)
    let playboard = [[Empty, X, O]]
    print playboard
    let game = ((P1, P2), playboard, 1)
    print (getCurrentTurnSymbol 1)
    print (getCurrentTurnSymbol 2)
    print (getCurrentTurnSymbol 3)
    print (checkLegalMove playboard (0,0))
    print "Check legal move:"
    print (markGrid game (0,0))
    print (markGrid game (0,1))
    print (markGrid game (0,2))
