{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module DataBus where

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as B
import GHC.Generics

data GameState = GameState
  { 
     classic :: ClassicGameState
  }
  deriving (Show, Generic)


data ClassicGameState = ClassicGameState
  { 
    finished :: Bool,
    today_character_id :: Int,
    valid_until :: Int
  }
  deriving (Show, Generic)

instance JSON.FromJSON ClassicGameState
instance JSON.ToJSON ClassicGameState
instance JSON.FromJSON GameState
instance JSON.ToJSON GameState

readGameState :: B.ByteString -> Either String GameState
readGameState = JSON.eitherDecode

writeGameState :: GameState -> B.ByteString
writeGameState = JSON.encode

