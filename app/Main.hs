module Main where

import           Game
import           Rendering
import           System.Console.ANSI
import           System.Random

runLoop :: RandomGen g => (GameBoard, Bool, Integer, Bool, g) -> IO ()
runLoop (board, _, score, True, _) = do
    clearScreen
    putStr "\n"
    putStr $ printBoard board
    putStrLn $ "Score: " ++ (show score)
    putStrLn $ "No more moves!"
runLoop (board, movementMade, score, gameFinished, g) = do
    clearScreen
    putStr "\n"
    putStr $ printBoard board
    putStrLn $ "Score: " ++ (show score)
    key <- getChar
    case key of
      'q' -> return ()
      'a' -> runLoop $ updateBoard g board MovementLeft score
      'w' -> runLoop $ updateBoard g board MovementUp score
      's' -> runLoop $ updateBoard g board MovementDown score
      'd' -> runLoop $ updateBoard g board MovementRight score

main :: IO ()
main = do
    stdGen <- newStdGen
    runLoop $ initGame stdGen
