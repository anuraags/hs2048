module Rendering where
import           Game

printTile :: Game.Tile -> [Char]
printTile EmptyTile  = (replicate 7 ' ')
printTile (Tile val) = (replicate numSpaces ' ') ++ show val
  where
    strLength = length (show val)
    numSpaces = 7 - strLength


printRow :: [Game.Tile] -> [Char]
printRow [a, b, c, d] = "|       |       |       |       |\n" ++
                       ("|" ++ (printTile a) ++  "|" ++ (printTile b) ++ "|" ++ (printTile c) ++ "|" ++ (printTile d) ++ "|\n") ++
                       "|       |       |       |       |"

printBoardRecursive :: (Num a, Eq a) => [[Game.Tile]] -> a -> [Char]
printBoardRecursive (x:xs) index = (printRow x) ++ "\n+-------+-------+-------+-------+\n" ++ printBoardRecursive xs (index + 1)
printBoardRecursive _ 4 = ""

printBoard :: GameBoard -> String
printBoard board = "+-------+-------+-------+-------+\n" ++ printBoardRecursive board 0
