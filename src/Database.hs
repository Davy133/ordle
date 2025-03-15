{-# LANGUAGE OverloadedStrings #-}

module Database ( getRandomCharacter, getAllCharacters, getCharacterById ) where

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

getRandomCharacter :: Config -> IO (Maybe M.Character)
getRandomCharacter config = withDB config $ \conn -> do
    r <- query_ conn "SELECT * FROM characters ORDER BY RANDOM() LIMIT 1" :: IO [M.Character]
    return $ listToMaybe r  

getAllCharacters :: Config -> IO [M.Character]
getAllCharacters config = withDB config $ \conn -> query_ conn "SELECT * FROM characters"

getCharacterById :: Config -> Int -> IO (Maybe M.Character)
getCharacterById config charId = withDB config $ \conn -> do
    r <- query conn "SELECT * FROM characters WHERE id = ?" (Only charId) :: IO [M.Character]
    return $ listToMaybe r
