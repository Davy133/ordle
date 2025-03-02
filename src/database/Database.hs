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

getCharacters :: Config -> IO [M.Character]
getCharacters config = do
  conn <- open (dbPath config)
  r <- query_ conn "SELECT * from characters" :: IO [M.Character]
  close conn
  return r

getRandomCharacter :: Config -> IO M.Character
getRandomCharacter config = do
  conn <- open (dbPath config)
  r <- query_ conn "SELECT * from characters ORDER BY RANDOM() LIMIT 1" :: IO [M.Character]
  close conn
  return $ head r

searchCharacters :: Config -> String -> IO [M.Character]
searchCharacters config searchString = do
  conn <- open (dbPath config)
  r <- query conn "SELECT * from characters WHERE name LIKE ?" (Only $ "%" ++ searchString ++ "%") :: IO [M.Character]
  close conn
  return r

searchMonsters :: Config -> String -> IO [M.Monster]
searchMonsters config searchString = do
  conn <- open (dbPath config)
  r <- query conn "SELECT * from monsters WHERE monsterName LIKE ?" (Only $ "%" ++ searchString ++ "%") :: IO [M.Monster]
  close conn
  return r



  