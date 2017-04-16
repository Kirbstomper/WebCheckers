--Author: Christopher Smith & Taylor White
--This is the logic meant to facilitate a checkers game 
--played by two people on the same computer
import qualified Data.Map as Map
import Data.Map (Map)
-- Create the types needed to faclitate a game of checkers
type Position = (Int, Int) -- Tuple representing grow column
type LocationMap = Map Position PieceType -- This is our in dictionary form

data Player = Player Char deriving (Show, Eq)
data Board = Board LocationMap deriving(Eq)
data PieceType = None | Red | Black | King PieceType deriving(Show, Eq)-- b,B, r,R for regulars and kings 
data Move = Move Position Position deriving (Show, Eq, Read)
data Game = Game GameTurn [Player] deriving (Show, Eq)
data GameTurn = GameTurn Board Player deriving (Show, Eq)
--Game Logic

    -- Return list of all positions on board
boardPositions = [(c, r) | r <- [1..8], c <- [1..8]]


--returns starting positions of red and black pieces
oddCol = (1,4) [(i*2) | i <-[1..4]]--columns where pieces are placed on every even position
evenCol = (1,4) [(i*2-1) | i <-[1..4]]--columns where pieces are placed on every odd position
--blackCol = genPos(6..8)
redPositions = [(c, r) | r <- [1..3], c <- [oddCol, evenCol, oddCol]]--black and red starting positions
blackPositions = [(c, r) | r <- [6..8], c <- [evenCol, oddCol, evenCol]]
    --Tells what is in a given space
inSpace :: LocationMap -> Position -> PieceType
inSpace lm p = maybe None id (Map.lookup p lm)


    --Swaps two spaces with each other (used in moving as a peice is replaced by a empty space when it moves)
swap :: LocationMap -> Position -> Position -> LocationMap
swap lm a b = new where
    mrk = inSpace lm -- Sets up the call to inSpace
    new = Map.insert a (mrk b) $ Map.insert b (mrk a) lm --Calls inserts whats current in pos a into pos b and vice versa
    --Lets us know if a position is actually on the board or not
inBounds :: Position -> Bool
inBounds pos = px > 0 && px<=8 && py > 0 && py <= 8 where
    (px,py) = pos

defBoard = Board defLocationMap -- Creates a board with a default location map(8x8)
defLocationMap = Map.fromList $ zip boardPositions (repeat None) --Maps every location on the board to none

    --returns a location and peice paired together
pairWithLocation :: PieceType -> Int -> Int -> (Position, Marker)
pairWithLocation p x y = ((x,y),p)

    --Returns a boards location map
peicesOnBoard :: Board -> LocationMap
peicesOnBoard (Board _ p) = p

    --updates pieces on a board
updatePieces :: Board -> LocationMap -> Board
updatePieces Board(_) lm = Board lm

    --Updates a specific position on the board with a new piece
updateLocation :: Board -> Position -> PieceType -> Board
updateLocation b pos p = updatedBoard where 
    ponb = peicesOnBoard b --Gets current location map
    updatedBoard = updatePieces b newLocationMap -- Creates new board where..
    newLocationMap = Map.insert pos p ponb -- Location map has peice at certain point

    -- returns all empty positions on the board
emptyPositions  :: Board -> [Position]
emptyPositions (Board locMap) = Map.keys $ Map.filter (==None) locMap
    -- Returns a non-king version of a piece
basicPiece :: PieceType -> PieceType
basicPiece (King p) = basicPiece p
basicPiece Red = Red
basicPiece Black = Black
basicPiece None = None

    -- Checks to see if anyone has won yet
hasWon :: Board -> PieceType -> Bool
hasWon _ None = Flase --Handles 
hasWon b (King peiceType) = hasWon b peiceType --Handles it being called using a king peice
hasWon (Board _ lMap) p = not otherPieceThere where
    otherPieceThere = any (eqType otherType) elems
    otherType = basicPiece p
    eqType p a = basicPiece a == p
    elems = Map.elems lMap
    
    -- Returns the current board for the game
gameBoard :: Game -> Board
gameBoard (Game(GameTurn board _)_) = board

    --Returns if a game has ended due to someone winning
gameEnded :: Game -> Bool
gameEnded game = any(hasWon board) pieces where
    board = gameBoard game
    pieces = [Red, Black]

    -- Returns the winner of a game
TheWinner :: Game -> PieceType
TheWinner game@(Game (GameTurn b ))

    -- Checks if a space is empty
IsEmptySpace :: Board -> Position -> Bool
IsEmptySpace b p  = space == None where
    space = markerAt (markers  b) 


    --Sees if a move can occur
    
    -- Makes a move 
preformMove :: Game -> Move -> Game



    -- Returns a list of possible legal moves from a given position
    
    
    -- returns a list of possible 
    --Promotes a given peice to a king 
Promote :: PieceType -> PieceType
Promote Red = King Red
Promote Black = King Black
Promote p = p

    --Determines if a peice should be kinged at its current position
CanKing :: PieceType -> Position -> Bool
CanKing King Red , _ = False -- Don't king if already one
CanKing Red, (_,1) = True -- King if a red peice reaches bottom row
CanKing Black, (_,8) = True -- King if Black peice reaches top row
CanKing _,_ = False -- Handles any other situation


--Game Tests