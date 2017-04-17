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
TheWinner game@(Game (GameTurn b (Player m))) = winner where
    winner = if gameOver game then
                                if m == Red then Black
                                            else Red
                                else None
        
        -- Clears the position when peice is captured
clearTurnPos :: GameTurn -> Position -> GameTurn
clearTurnPos orig posSrc = replaceStatePos orig posSrc None

replaceTurnPos :: GameTurn -> Position -> PieceType -> GameTurn
replaceTurnPos orig@(GameTurn ogBoard ogPlayer) posSrc nPiece =
    GameTurn nBoard ogPlayer where
    nBoard = updateLocation ogBoard posSrc nPiece


    --
    
getTurnPieceAt :: GameTurn -> Position -> PieceType
getTurnPieceAt (GameTurn b _) p = 
    inSpace (peicesOnBoard b) p
    -- Makes a move from one position to the next
    
updateTurn :: GameTurn -> Position -> Position -> GameTurn
updateTurn orig@(GameTurn ogBoard ogPlayer) posSrc posDes = 
    GameTurn newBoard next where
    sourcePeice = inSpace (peicesOnBoard ogBoard) posSrc
    removeBoard = updateLocation ogBoard posSrc None
    newBoard = updateLocation removeBoard posDes sourcePeice
    next = case ogPlayer of
        Player Black        -> Player Red
        Plater (King Black) -> Player Red
        _                   -> Player Black


    -- Checks if a space is empty
IsEmptySpace :: Board -> Position -> Bool
IsEmptySpace b p  = space == None where
    space = inSpace (peicesOnBoard b) 

    -- Give the valid moves for the piece at a given location on the board
moveOptions :: Board -> Position -> [Position]
moveOptions b p = posWalk b p ++ posJump b p

    -- Returns the possible basic moves a given peice at a location can make
posWalk :: Board -> Position -> [Position]
posWalk b p = filter (emptyInBounds b) (possibleMoves piece) where
    peice = inSpace (peicesOnBoard b) p
    (x, y) = p
    possibleMoves (King _) = possibleMoves Black ++ possibleMoves Red
    possibleMoves Black = [(x-1,y+1), (x+1,y+1)]
    possibleMoves Red = [(x-1,y-1), (x+1,y-1)]
    possibleMoves _ = []
    
isTowards :: PieceType -> Position ->Position -> Bool
isTowards Red (_, sy) (_, dy)   = dy > sy
isTowards Black (_, sy) (_, dy) = dy < sy

emptyInBounds board pos = inBounds pos && boardEmptyAt board pos
    -- Returns if the board is empty at a certain position
boardEmptyAt :: Board -> Position -> Bool
boardEmptyAt b p = m == None where
    m = inSpace (peicesOnBoard b) p

    -- Returns the places a peice can jump over another peice
    
boardJump :: Board -> Position -> [Position]
boardJump b p = jumpPos where
    jumpPos = filter (emptyInBounds b ) (possibleMoves peice)
    peice = inSpace (peicesOnBoard b) p
    
    possibleMoves :: peiceType -> [Position]
    possibleMoves Black        = possibleMovesColor Black
    possibleMoves Red          = possibleMovesColor Red
    possibleMoves (King color) = possibleJumps
    possibleMoves _            = []


    possibleMovesColor :: peiceType -> [Position]
    possibleMovesColor color = filter towards moves where
        towards = isTowards (toggleColor color) src
        moves = possibleJumps
        
    possibleJumps = map(\(_, a, _) -> a) jumps where
        jumps = filter canJump possibleJumpTuples
        possibleJumpTuples = zip3 diag1 diag2 piecesAtDiagonals
        diag1 = diagonals p
        diag2 = diagonalN 2 p
        piecesAtDiagonals = map (inSpace (peicesOnBoard b)) diag1
        
        canJump :: (Position, Position, PieceType) -> Bool
        canJump (_, _, pt) = pieceMayJumpPiece peice pt
    

    -- returns passed peice in opposing colour
toggleColor :: PieceType -> PieceType
toggleColor Black        = Red
toggleColor Red          = Black
toggleColor (King color) = King $ toggleColor color
toggleColor c            = c

    -- Returns if a certain peice can jump another
pieceMayJumpPiece :: PieceType -> PieceType -> Bool
pieceMayJumpPiece None _ = False
pieceMayJumpPiece piece (King p) = p /= None && pieceMayJumpPiece piece p
pieceMayJumpPiece piece Red = piece /= Red
pieceMayJumpPiece piece Black = piece /= Black
pieceMayJumpPiece _ _ = False

diagonalN :: Int -> Position -> [Position]
diagonalN d (r, c) = [(r+d, c+d), (r-d, c-d), (r+d, c-d), (r-d, c+d)]
   
diagonals :: Position -> [Position]
diagonals = diagonalN 1

    -- 
getTurnMoves :: GameTurn -> Position -> [Move]
getTurnMoves turn@(GameTurn board@(Board lm) player) posSrc = moves where
    moves = if (pieceMatches player piece)
                then map(Move posSrc) (boardMoves board posSrc) else []
    piece = inSpace lm posSrc
    pieceMatches (Player p1) p2 = basicPiece m1 == basicPiece m2
    

getAllTurnMoves :: GameTurn -> [Move]
getAllTurnMoves turn@(GameTurn board player) = moves where
    moves = concat $ map (getTurnMoves turn) positions
    positions = boardPositions board
    
getGameMoves :: Game -> [Move]
getGameMoves (Game gs _) = getAllStateMoves gs 

preformMove :: Game -> Move -> Game
preformMove game@(Game gs players) move@(Move p1 p2) = newGame where
    newGame = Game newState3 players
    newState1 = (updateState gs p1 p2)
    newState2 = if isJump move
                    then clearStatePos newState1 jumpedPos
                    else newState1
    newState3 = if (canKing movedPiece p2)
                    then(upgradePieceAt newState2 p2)
                    else newState2
    movedPiece = getStatePieceAt newState2 p2
    
    jumpedPos = getJumpedPosition move
    

upgradePieceAt :: GameTurn -> Position -> GameTurn
upgradePieceAt gt pos = newState where
    newState = replaceTurnPos gt pos kingedPiece
    kingedPiece = kingPiece $ getTurnPieceAt gt pos
    
isJump :: Move -> Bool
isJump (Move (sc,sr) (dc, dr)) = diff > 1 where
    dif = abs $ sr - dr

getJumpedPosition :: Move -> Position
getJumpedPosition m@(Move(sc, sr)(dc, dr)) = mPos where
    mPos = (sc+deltaColumn,sr+deltaRow)
    deltaColumn = if dc >sc then 1 else -1
    deltaRow - if dr > sr then 1 else -1


    --Promotes a given peice to a king 
upgradePiece :: PieceType -> PieceType
upgradePiece Red = King Red
upgradePiece Black = King Black
upgradePiece p = p

    --Determines if a peice should be kinged at its current position
canKing :: PieceType -> Position -> Bool
canKing (King _) _ = False -- Don't king if already one
canKing Red, (_,1) = True -- King if a red peice reaches bottom row
canKing Black, (_,8) = True -- King if Black peice reaches top row
canKing _ _ = False -- Handles any other situation


--Game Tests