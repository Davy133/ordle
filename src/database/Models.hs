{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Models (Character(..), Monster(..)) where

import GHC.Generics(Generic)
import Database.SQLite.Simple (FromRow, fromRow, field)

data Character = Character { 
    characterId :: Int,
    name :: String,
    age :: String,
    status :: String,
    association :: String,
    first_appearance :: String,
    actor :: String,
    affinity :: String,
    gender :: String
  }  deriving (Eq, Show, Generic)


data Monster = Monster { 
    monsterId :: Int,
    monsterName :: String,
    image :: String
  } deriving (Eq, Show, Generic)


instance FromRow Character where
  fromRow = Character <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance FromRow Monster where
  fromRow = Monster <$> field <*> field <*> field

