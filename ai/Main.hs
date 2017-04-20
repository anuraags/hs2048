{-# LANGUAGE InstanceSigs #-}
module Main where

import           Control.Concurrent
import           Game
import           Rendering
import           System.Console.ANSI
import           System.Random

class GameAi ai where
  run :: ai -> GameBoard -> (MovementDirection, ai)

data MoveUpAi = MoveUpAi
instance GameAi MoveUpAi where
  run MoveUpAi _ = (MovementUp, MoveUpAi)

data MoveRandomAi = MoveRandomAi
  { randomGenerator :: StdGen
  }

moveFromIndex :: Int -> MovementDirection
moveFromIndex 0 = MovementUp
moveFromIndex 1 = MovementLeft
moveFromIndex 2 = MovementRight
moveFromIndex 3 = MovementDown

generateRandomMove :: (RandomGen g) => g -> (MovementDirection, g)
generateRandomMove randGen = (moveFromIndex index, randGen2)
  where
   (index, randGen2) = randomR (0, 3) randGen

instance GameAi MoveRandomAi where
  run ai _ = (move, MoveRandomAi { randomGenerator = randomGenerator2 })
    where
      (move, randomGenerator2) = generateRandomMove (randomGenerator ai)

data MoveClockwiseAi = MoveClockwiseAi
  { lastMove :: MovementDirection
  }
generateNextClockwiseMove move
  | move == MovementUp = MovementRight
  | move == MovementRight = MovementDown
  | move == MovementDown = MovementLeft
  | otherwise = MovementUp

instance GameAi MoveClockwiseAi where
  run ai _ = (move, MoveClockwiseAi { lastMove = move })
    where
      move = generateNextClockwiseMove (lastMove ai)

updateBlockedMovements movementsMade True  = 0
updateBlockedMovements movementsMade False = movementsMade + 1

runAi :: (RandomGen g, GameAi ai) => (GameBoard, Integer, Integer, Bool, g, ai) -> IO ()
runAi (board, _, score, True, _, _) = do
    clearScreen
    putStr "\n"
    putStr $ printBoard board
    putStrLn $ "No more moves!"
    putStrLn $ "Your score is: " ++ (show score)
runAi (board, blockedMovements, score, gameFinished, g, gameAi)
  | blockedMovements > 10 =  do
      clearScreen
      putStr "\n"
      putStr $ printBoard board
      putStrLn $ "Your AI is blocked for more than 10 moves!"
      putStrLn $ "Your score is: " ++ (show score)
  | otherwise = do
      clearScreen
      putStr "\n"
      putStr $ printBoard board
      putStrLn $ "Score: " ++ (show score)
      threadDelay 500000
      runAi (updatedBoard, updatedBlockedMovements, updatedScore, gameFinished, g2, newAi)
      where
        (aiMove, newAi) = run gameAi board
        (updatedBoard, movementMade, updatedScore, gameFinished, g2) = updateBoard g board aiMove score
        updatedBlockedMovements = updateBlockedMovements blockedMovements movementMade


initGameWithAi :: (RandomGen g, GameAi ai) => (GameBoard, Bool, Integer, Bool, g) -> ai -> (GameBoard, Integer, Integer, Bool, g, ai)
initGameWithAi (board, _, score, gameFinished, randGen) gameAi
  = (board, 0, score, gameFinished, randGen, gameAi)

main :: IO ()
main = do
  stdGen1 <- newStdGen
  stdGen2 <- newStdGen
  --runAi $ initGameWithAi (initGame stdGen1) (MoveRandomAi { randomGenerator = stdGen2 })
  runAi $ initGameWithAi (initGame stdGen1) (MoveClockwiseAi { lastMove = MovementUp })
