{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Models (Character(..), Monster(..)) where

import GHC.Generics(Generic)
import Database.SQLite.Simple (FromRow, fromRow, field, ToRow, toRow)

data Character = Character { 
    characterId :: Int,
    name :: String,
    age :: String,
    status :: String,
    association :: String,
    first_appearance :: String,
    actor :: String,
    affinity :: String,
    gender :: String,
    emojis :: String,
    imageChar :: String,
    quote :: String
  }  deriving (Eq, Show, Generic)

data Monster = Monster { 
    monsterId :: Int,
    monsterName :: String,
    image :: String
  } deriving (Eq, Show, Generic)

instance FromRow Character where
  fromRow = Character <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance FromRow Monster where
  fromRow = Monster <$> field <*> field <*> field

instance ToRow Character where
  toRow Character{..} = toRow (characterId, name, age, status, association, first_appearance, actor, affinity)

instance ToRow Monster where
  toRow Monster{..} = toRow (monsterId, monsterName, image)

