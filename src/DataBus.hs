{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module DataBus where

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as B
import GHC.Generics

-- Define the GameState data structure
data GameState = GameState
  { 
     classic :: ClassicGameState,
     blacklist :: [Int]  -- Added blacklist field
  }
  deriving (Show, Generic)

-- Define the ClassicGameState data structure
data ClassicGameState = ClassicGameState
  { 
    finished :: Bool,
    today_character_id :: Int,
    valid_until :: Int
  }
  deriving (Show, Generic)

-- JSON instances for automatic serialization/deserialization
instance JSON.FromJSON ClassicGameState
instance JSON.ToJSON ClassicGameState
instance JSON.FromJSON GameState
instance JSON.ToJSON GameState

-- Helper function to get the game state from the JSON file
getJSON :: IO (Either String [GameState])
getJSON = JSON.eitherDecode <$> B.readFile "game_state.json"

-- Helper function to write the game state to the JSON file
writeJSON :: [GameState] -> IO ()
writeJSON = B.writeFile "game_state.json" . JSON.encode
