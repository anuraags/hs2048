import           Game
import           Math
import           System.Random
import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "initialise empty game" $ do
    it "returns an empty game" $
      initEmptyGame `shouldBe` (replicate 4 $ replicate 4 EmptyTile)

  describe "is tile empty" $ do
    let game1 = initEmptyGame
    let game2 = [[ EmptyTile, Tile 4, EmptyTile, EmptyTile ]
          , [ Tile 2, EmptyTile, EmptyTile, EmptyTile ]
          , [ EmptyTile, EmptyTile, EmptyTile, EmptyTile ]
          , [ EmptyTile, EmptyTile, EmptyTile, EmptyTile ]]
    it "returns true for an empty tile" $
      isTileEmpty game1 0 0 `shouldBe` True
    it "returns false for a non-empty tile" $
      isTileEmpty game2 0 1 `shouldBe` False
    it "returns false for a non-empty tile" $
      isTileEmpty game2 1 0 `shouldBe` False
    it "returns false for an out of bounds tile" $
      isTileEmpty game2 10 10 `shouldBe` False

  describe "put tile" $ do
    it "puts the correct tile" $
      let game1 = putTile initEmptyGame 2 2 8
          game2 = [[ EmptyTile, EmptyTile, EmptyTile, EmptyTile ]
            , [ EmptyTile, EmptyTile, EmptyTile, EmptyTile ]
            , [ EmptyTile, EmptyTile, Tile 8, EmptyTile ]
            , [ EmptyTile, EmptyTile, EmptyTile, EmptyTile ]]
      in game1 `shouldBe` game2

  describe "generate random empty tile" $ do
    it "always returns an empty tile" $
      let (row, col, _) = getRandomEmptyTile (mkStdGen 35) game3
          game3 = [[ Tile 2, Tile 4, Tile 2, Tile 2 ]
            , [ Tile 2, EmptyTile, Tile 2, Tile 2 ]
            , [ Tile 2, Tile 2, Tile 2, Tile 2 ]
            , [ Tile 2, Tile 2, Tile 2, Tile 2 ]]
      in (row, col) `shouldBe` (1, 1)

  describe "init game" $ do
    it "always returns the same board with the same random number generator" $
      let gameExpected = putTile (putTile initEmptyGame 0 0 2) 3 1 4
          (gameResult, movementMade, score, _) = (initGame (mkStdGen 35))
      in (gameExpected, movementMade, score) `shouldBe` (gameResult, False, 0)

  describe "gravitate line left" $ do
    it "shifts tiles left until they can't move" $
      gravitateLineLeft [Tile 2, EmptyTile, EmptyTile, EmptyTile] 0 `shouldBe` ([Tile 2, EmptyTile, EmptyTile, EmptyTile], 0)
    it "shifts tiles left until they can't move" $
      gravitateLineLeft [EmptyTile, EmptyTile, EmptyTile, Tile 2] 0 `shouldBe` ([Tile 2, EmptyTile, EmptyTile, EmptyTile], 0)
    it "shifts tiles left until they encounter a non-empty tile of different value" $
      gravitateLineLeft [Tile 2, EmptyTile, EmptyTile, Tile 4] 0 `shouldBe` ([Tile 2, Tile 4, EmptyTile, EmptyTile], 0)
    it "merges tiles with the same value for a double value tile 1" $
      gravitateLineLeft [EmptyTile, Tile 4, EmptyTile, Tile 4] 0 `shouldBe` ([Tile 8, EmptyTile, EmptyTile, EmptyTile], 8)
    it "merges tiles with the same value for a double value tile 2" $
      gravitateLineLeft [Tile 2, EmptyTile, Tile 2, Tile 4] 0 `shouldBe` ([Tile 4, Tile 4, EmptyTile, EmptyTile], 4)
    it "fills empty tiles at the end" $
      gravitateLineLeft [EmptyTile, Tile 2, Tile 4, EmptyTile] 0 `shouldBe` ([Tile 2, Tile 4, EmptyTile, EmptyTile], 0)
    it "fills empty tiles at the end with merge" $
      gravitateLineLeft [EmptyTile, Tile 2, EmptyTile, Tile 2] 0 `shouldBe` ([Tile 4, EmptyTile, EmptyTile, EmptyTile], 4)
    it "only merges the first occurence of a pair" $
      gravitateLineLeft [Tile 2, Tile 2, Tile 2, Tile 2] 0 `shouldBe` ([Tile 4, Tile 4, EmptyTile, EmptyTile], 8)

  describe "gravitate board left" $ do
    it "shifts tiles left on board" $
      let game = [[Tile 2, EmptyTile, EmptyTile, EmptyTile]
                , [Tile 2, EmptyTile, EmptyTile, Tile 4]
                , [EmptyTile, Tile 4, EmptyTile, Tile 4]
                , [Tile 2, EmptyTile, Tile 2, Tile 4]]
          gameGravitated = [[Tile 2, EmptyTile, EmptyTile, EmptyTile]
              , [Tile 2,Tile 4, EmptyTile, EmptyTile]
              , [Tile 8, EmptyTile, EmptyTile, EmptyTile]
              , [Tile 4,Tile 4, EmptyTile, EmptyTile]]
      in gravitateBoardLeft game 0 `shouldBe` (gameGravitated, True, 12)

  describe "rotate board clockwise" $ do
    it "rotates the board clockwise" $
      let game = [[Tile 1, Tile 2, Tile 3, Tile 4]
              , [Tile 5, Tile 6, Tile 7, Tile 8]
              , [Tile 9, Tile 10, Tile 11, Tile 12]
              , [Tile 13, Tile 14, Tile 15, Tile 16]
                ]
          gameRotated = [[Tile 13, Tile 9, Tile 5, Tile 1]
              , [Tile 14, Tile 10, Tile 6, Tile 2]
              , [Tile 15, Tile 11, Tile 7, Tile 3]
              , [Tile 16, Tile 12, Tile 8, Tile 4]
                ]
      in rotateBoardClockwise game `shouldBe` gameRotated

  describe "rotate board counter clocksiwse" $ do
    it "rotates the board counter clockwise" $
      let game = [[Tile 13, Tile 9, Tile 5, Tile 1]
              , [Tile 14, Tile 10, Tile 6, Tile 2]
              , [Tile 15, Tile 11, Tile 7, Tile 3]
              , [Tile 16, Tile 12, Tile 8, Tile 4]
                ]
          gameRotated = [[Tile 1, Tile 2, Tile 3, Tile 4]
              , [Tile 5, Tile 6, Tile 7, Tile 8]
              , [Tile 9, Tile 10, Tile 11, Tile 12]
              , [Tile 13, Tile 14, Tile 15, Tile 16]
                ]
      in rotateBoardCounterClockwise game `shouldBe` gameRotated
