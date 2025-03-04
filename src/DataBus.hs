{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module DataBus (
    GameState(..),
    ModeState(..),
    getJSON,
    writeJSON
) where

import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import System.Directory

-- Data structure for storing mode-specific state
data ModeState = ModeState
    { finished :: Bool
    , today_character_id :: Int
    , valid_until :: Int
    } deriving (Show, Generic)

instance FromJSON ModeState
instance ToJSON ModeState

-- Main game state structure
data GameState = GameState
    { blacklist :: [Int]
    , classic :: ModeState
    , emoji :: ModeState
    , quotes :: ModeState
    } deriving (Show, Generic)

instance FromJSON GameState
instance ToJSON GameState

-- File path for storing game state
filePath :: FilePath
filePath = "gameState.json"

-- Function to read JSON game state from file
getJSON :: IO (Either String [GameState])
getJSON = do
    exists <- doesFileExist filePath
    if exists
        then eitherDecode <$> B.readFile filePath
        else return (Right [])

-- Function to write JSON game state to file
writeJSON :: [GameState] -> IO ()
writeJSON gameState = B.writeFile filePath (encode gameState)