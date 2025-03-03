{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Control.Monad.IO.Class (liftIO)
import DataBus (GameState(..), ClassicGameState(..), writeJSON, getJSON)
import ClassicController
import Models (Character(..))
import Control.Exception (bracket)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as B

-- Mock database and helpers to simulate file reading/writing
mockGameStateFile :: FilePath
mockGameStateFile = "mock_game_state.json"

-- Helper function to create a mock game state
mockGameState :: [GameState]
mockGameState =
  [ GameState
      { classic = ClassicGameState { finished = False, today_character_id = 1, valid_until = 60742 }
      , blacklist = []
      }
  ]

-- Helper to write mock data to the file
writeMockData :: IO ()
writeMockData = writeJSON mockGameState

-- Helper to clear mock data
clearMockData :: IO ()
clearMockData = B.writeFile mockGameStateFile ""

main :: IO ()
main = hspec $ do
  describe "ClassicController" $ do
    before writeMockData $ do
      it "selectCurrentCharacter selects a character for the day" $ do
        -- Assuming selectCurrentCharacter modifies the file
        selectCurrentCharacter
        gameStateResult <- getJSON
        case gameStateResult of
          Left err -> fail err
          Right (gs:_) -> do
            -- Check if today_character_id is set and valid
            todayCharacterId <- return (today_character_id (classic gs))
            todayCharacterId `shouldBe` 1

    it "selectCurrentCharacter adds the selected character to the blacklist" $ do
      selectCurrentCharacter
      gameStateResult <- getJSON
      case gameStateResult of
        Left err -> fail err
        Right (gs:_) -> do
          let blacklist = blacklist gs
          blacklist `shouldBe` [1]

    it "flipFinished sets finished to True" $ do
      flipFinished
      gameStateResult <- getJSON
      case gameStateResult of
        Left err -> fail err
        Right (gs:_) -> do
          let finished = finished (classic gs)
          finished `shouldBe` True

    it "getTodayCharacter returns the correct character" $ do
      let character = Character 1 "Test Character" 30 "Alive" "Test Group" "2020-01-01" "Test Actor" "Friendship" "Male"
      gameStateResult <- getJSON
      case gameStateResult of
        Left err -> fail err
        Right (gs:_) -> do
          todayCharacter <- getTodayCharacter
          case todayCharacter of
            Just c -> name c `shouldBe` "Test Character"
            Nothing -> fail "Character not found"

  describe "DataBus" $ do
    it "getJSON reads the game state correctly" $ do
      gameStateResult <- getJSON
      case gameStateResult of
        Left err -> fail err
        Right (gs:_) -> do
          let todayCharacterId = today_character_id (classic gs)
          todayCharacterId `shouldBe` 1

    it "writeJSON writes the game state to file" $ do
      let newGameState = [GameState (ClassicGameState False 2 60743) [2]]
      writeJSON newGameState
      gameStateResult <- getJSON
      case gameStateResult of
        Left err -> fail err
        Right (gs:_) -> do
          let todayCharacterId = today_character_id (classic gs)
          todayCharacterId `shouldBe` 2
