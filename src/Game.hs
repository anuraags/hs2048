module Game where
import           Data.List
import           Debug.Trace
import           System.Random

data MovementDirection = MovementUp | MovementDown | MovementLeft | MovementRight deriving (Show, Eq)
data Tile = Tile Integer | EmptyTile deriving (Show, Eq)
type GameBoard = [[Tile]]
type RowType = Int
type ColType = Int

initEmptyGame :: GameBoard
initEmptyGame = (replicate 4 $ replicate 4 EmptyTile)

getColumnTile :: [Tile] -> ColType -> Maybe Tile
getColumnTile (x:xs) 0   = Just x
getColumnTile (x:xs) col = getColumnTile xs (col - 1)
getColumnTile [] col     = Nothing

getTile :: GameBoard -> RowType -> ColType -> Maybe Tile
getTile (x:xs) 0 col   = getColumnTile x col
getTile (x:xs) row col = getTile (xs) (row - 1) col
getTile [] row col     = Nothing

putTileOnLine :: [Tile] -> ColType -> Integer -> [Tile]
putTileOnLine (_:xs) 0 val   = ((Tile val):xs)
putTileOnLine (x:xs) col val = (x:(putTileOnLine xs (col - 1) val))
putTileOnLine [] col val     = []

putTile :: GameBoard -> RowType -> ColType -> Integer -> GameBoard
putTile (x:xs) 0 col val   = ((putTileOnLine x col val):xs)
putTile (x:xs) row col val = (x:(putTile xs (row - 1) col val))
putTile [] row col val     = []


isTileEmpty :: GameBoard -> RowType -> ColType -> Bool
isTileEmpty board row col = (tile == Just EmptyTile)
  where
    tile = getTile board row col

getRandomTile :: (RandomGen g) => g -> (Int, Int, g)
getRandomTile randGen1 = (row, col, randGen3)
  where
    (row, randGen2) = randomR (0, 3) randGen1
    (col, randGen3) = randomR (0, 3) randGen2

getRandomEmptyTile :: (RandomGen g) => g -> GameBoard -> (RowType, ColType, g)
getRandomEmptyTile randGen board = if (isTileEmpty board row col)
  then (row, col, newRandGen)
  else getRandomEmptyTile newRandGen board
  where
    (row, col, newRandGen) = getRandomTile randGen
    tileIsEmpty = isTileEmpty board row col

generateRandomTileValue :: (RandomGen g) => g -> (Integer, g)
generateRandomTileValue randGen1
  | randomNumber < 90 = (2, randGen2)
  | otherwise = (4, randGen2)
  where (randomNumber, randGen2) = randomR (0 :: Integer, 100 :: Integer) randGen1

fillRandomEmptyTile :: (RandomGen g) => g -> GameBoard -> (GameBoard, g)
fillRandomEmptyTile randGen1 board = (updatedBoard, randGen2)
  where
    (row, col, randGen2) = getRandomEmptyTile randGen1 board
    (val, randGen3) = generateRandomTileValue randGen2
    updatedBoard = putTile board row col val


initGame :: (RandomGen g) => g -> (GameBoard, Bool, Integer, g)
initGame randGen1 = (game3, False, 0, randGen3)
  where
    emptyGame = initEmptyGame
    (game2, randGen2) = fillRandomEmptyTile randGen1 emptyGame
    (game3, randGen3) = fillRandomEmptyTile randGen2 game2


gravitateNonEmptyTilesLeft :: [Tile] -> Integer -> ([Tile], Integer)
gravitateNonEmptyTilesLeft (Tile val1:Tile val2:xs) score
  | val1 == val2 = (Tile (val1 + val2):gravitated1, updatedScore1)
  | otherwise = (Tile val1:gravitated2, updatedScore2)
  where
    (gravitated1, updatedScore1) = gravitateNonEmptyTilesLeft xs (score + (val1 + val2))
    (gravitated2, updatedScore2) = gravitateNonEmptyTilesLeft (Tile val2:xs) score
gravitateNonEmptyTilesLeft [tile] score = ([tile], score)
gravitateNonEmptyTilesLeft [] score = ([], score)

pad :: Int -> [Tile] -> [Tile]
pad 0 _      = []
pad n []     = (EmptyTile:(pad (n-1) []))
pad n (x:xs) = (x:pad (n-1) xs)

gravitateLineLeft line score = (pad 4 gravitated, updatedScore)
  where
    (gravitated, updatedScore) = gravitateNonEmptyTilesLeft (filter (/= EmptyTile) line) score

gravitateBoardLeft :: GameBoard -> Integer -> (GameBoard, Bool, Integer)
gravitateBoardLeft (x:xs) score = ((lineGravitated:remainingBoard), (lineGravitated /= x) || movementMade, updatedScore2)
  where
    (lineGravitated, updatedScore1) = gravitateLineLeft x score
    (remainingBoard, movementMade, updatedScore2) = gravitateBoardLeft xs updatedScore1
gravitateBoardLeft [] score    = ([], False, score)

rotateBoardCounterClockwise :: GameBoard -> GameBoard
rotateBoardCounterClockwise = reverse . transpose


rotateBoardClockwise :: GameBoard -> GameBoard
rotateBoardClockwise = transpose . reverse

rotateBoard180 :: GameBoard -> GameBoard
rotateBoard180 = rotateBoardClockwise . rotateBoardClockwise

makeMove :: GameBoard -> MovementDirection -> Integer -> (GameBoard, Bool, Integer)
makeMove board MovementLeft score = (gravitated, movementMade, updatedScore)
  where (gravitated, movementMade, updatedScore) = gravitateBoardLeft board score
makeMove board MovementRight score = (rotateBoard180 gravitated, movementMade, updatedScore)
  where (gravitated, movementMade, updatedScore) = gravitateBoardLeft (rotateBoard180 board) score
makeMove board MovementUp score =  (rotateBoardClockwise gravitated, movementMade, updatedScore)
  where (gravitated, movementMade, updatedScore) = gravitateBoardLeft (rotateBoardCounterClockwise board) score
makeMove board MovementDown score =  (rotateBoardCounterClockwise gravitated, movementMade, updatedScore)
  where (gravitated, movementMade, updatedScore) = gravitateBoardLeft (rotateBoardClockwise board) score



updateBoard :: (RandomGen g) => g -> GameBoard -> MovementDirection -> Integer -> (GameBoard, Bool, Integer, g)
updateBoard randGen1 game movement score
  | movementMade = (game2, True, updatedScore, randGen2)
  | otherwise = (game1, False, updatedScore, randGen1)
  where
    (game1, movementMade, updatedScore) = makeMove game movement score
    (game2, randGen2) = fillRandomEmptyTile randGen1 game1

