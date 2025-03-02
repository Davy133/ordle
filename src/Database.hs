{-# LANGUAGE OverloadedStrings #-}

module Database ( getMonsters, getRandomMonster) where

import qualified Models as M
import           Database.SQLite.Simple
import           Config
-- import           Control.Applicative
-- import           Database.SQLite.Simple.FromRow

getMonsters :: Config -> IO [M.Monster]
getMonsters config = do
  conn <- open (dbPath config)
  r <- query_ conn "SELECT * from monsters" :: IO [M.Monster]
  close conn
  return r

getRandomMonster :: Config -> IO M.Monster
getRandomMonster config = do
  conn <- open (dbPath config)
  r <- query_ conn "SELECT * from monsters ORDER BY RANDOM() LIMIT 1" :: IO [M.Monster]
  close conn
  return $ head r 
