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


getEmptyTilesInLine (EmptyTile:xs) row col = (row, col):getEmptyTilesInLine xs row (col + 1)
getEmptyTilesInLine (x:xs) row col = getEmptyTilesInLine xs row (col + 1)
getEmptyTilesInLine [] _ _ = []

getAllEmptyTilesRecursive :: GameBoard -> RowType -> [(RowType, ColType)]
getAllEmptyTilesRecursive (x:xs) row = (getEmptyTilesInLine x row 0) ++ getAllEmptyTilesRecursive xs (row + 1)
getAllEmptyTilesRecursive [] _ = []

getAllEmptyTiles :: GameBoard -> [(RowType, ColType)]
getAllEmptyTiles board = getAllEmptyTilesRecursive board 0


getRandomTileFromList :: (RandomGen g) => g -> [(RowType, ColType)] -> (Maybe (RowType, ColType), g)
getRandomTileFromList randGen [] = (Nothing, randGen)
getRandomTileFromList randGen listOfTiles = (Just (row, col), newRandGen)
  where
    (randIndex, newRandGen) = randomR (0, (length listOfTiles) - 1) randGen
    (row, col) = listOfTiles!!randIndex

getRandomEmptyTile :: (RandomGen g) => g -> GameBoard -> (Maybe (RowType, ColType), g)
getRandomEmptyTile randGen board = getRandomTileFromList randGen (getAllEmptyTiles board)

generateRandomTileValue :: (RandomGen g) => g -> (Integer, g)
generateRandomTileValue randGen1
  | randomNumber < 90 = (2, randGen2)
  | otherwise = (4, randGen2)
  where (randomNumber, randGen2) = randomR (0 :: Integer, 100 :: Integer) randGen1


updateBoardWithTile :: (RandomGen g) => g -> GameBoard -> Maybe (RowType, ColType) -> (GameBoard, Bool, g)
updateBoardWithTile randGen board Nothing = (board, True, randGen)
updateBoardWithTile randGen board (Just (row, col)) = (updatedBoard, False, newRandGen)
  where
    (val, newRandGen) = generateRandomTileValue randGen
    updatedBoard = putTile board row col val

fillRandomEmptyTile :: (RandomGen g) => g -> GameBoard -> (GameBoard, Bool, g)
fillRandomEmptyTile randGen1 board = (updatedBoard, gameFinished, randGen2)
  where
    (maybeTile, randGen2) = getRandomEmptyTile randGen1 board
    (updatedBoard, gameFinished, randGen3) = updateBoardWithTile randGen2 board maybeTile


initGame :: (RandomGen g) => g -> (GameBoard, Bool, Integer, Bool, g)
initGame randGen1 = (game3, True, 0, False, randGen3)
  where
    emptyGame = initEmptyGame
    (game2, _, randGen2) = fillRandomEmptyTile randGen1 emptyGame
    (game3, _, randGen3) = fillRandomEmptyTile randGen2 game2


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



updateBoard :: (RandomGen g) => g -> GameBoard -> MovementDirection -> Integer -> (GameBoard, Bool, Integer, Bool, g)
updateBoard randGen1 game movement score
  | movementMade = (game2, True, updatedScore, gameFinished, randGen2)
  | otherwise = (game1, False, updatedScore, gameFinished, randGen1)
  where
    (game1, movementMade, updatedScore) = makeMove game movement score
    (game2, gameFinished, randGen2) = fillRandomEmptyTile randGen1 game1

