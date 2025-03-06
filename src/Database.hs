{-# LANGUAGE OverloadedStrings #-}

module Database ( getRandomCharacter, getAllCharacters, getCharacterById, getCharactersWithQuotes, getCharactersWithEmoji, getRandomMonster, getAllMonsters, getMonsterById) where

import qualified Models as M
import Database.SQLite.Simple
    ( Connection, close, open, query_, query, Only (Only) )
import Config ( Config(dbPath) )
import Control.Exception (catch, SomeException)
import Data.Maybe (listToMaybe)

withDB :: Config -> (Connection -> IO a) -> IO a
withDB config action = do
  conn <- open $ dbPath config
  result <- action conn `catch` handleDBError
  close conn
  return result

handleDBError :: SomeException -> IO a
handleDBError e = do
    putStrLn $ "Database error: " ++ show e
    error "Database operation failed"

getRandomMonster :: Config -> IO (Maybe M.Monster)
getRandomMonster config = withDB config $ \conn -> do
    r <- query_ conn "SELECT * FROM monsters ORDER BY RANDOM() LIMIT 1" :: IO [M.Monster]
    return $ listToMaybe r

getAllMonsters :: Config -> IO [M.Monster]
getAllMonsters config = withDB config $ \conn -> query_ conn "SELECT * FROM monsters"

getMonsterById :: Config -> Int -> IO (Maybe M.Monster)
getMonsterById config monsterId = withDB config $ \conn -> do
    r <- query conn "SELECT * FROM monsters WHERE monsterId = ?" (Only monsterId) :: IO [M.Monster]
    return $ listToMaybe r

getRandomCharacter :: Config -> IO (Maybe M.Character)
getRandomCharacter config = withDB config $ \conn -> do
    r <- query_ conn "SELECT * FROM characters ORDER BY RANDOM() LIMIT 1" :: IO [M.Character]
    return $ listToMaybe r

getAllCharacters :: Config -> IO [M.Character]
getAllCharacters config = withDB config $ \conn -> query_ conn "SELECT * FROM characters"

getCharacterById :: Config -> Int -> IO (Maybe M.Character)
getCharacterById config charId = withDB config $ \conn -> do
    r <- query conn "SELECT * FROM characters WHERE characterId = ?" (Only charId) :: IO [M.Character]
    return $ listToMaybe r

getCharactersWithQuotes :: Config -> IO [M.Character]
getCharactersWithQuotes config = withDB config $ \conn -> do
    query_ conn "SELECT * FROM characters WHERE quote IS NOT NULL AND quote != ''" :: IO [M.Character]

getCharactersWithEmoji :: Config -> IO [M.Character]
getCharactersWithEmoji config = withDB config $ \conn -> do
    query_ conn "SELECT * FROM characters WHERE emojis IS NOT NULL AND emojis != ''" :: IO [M.Character]

