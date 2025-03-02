{-# LANGUAGE OverloadedStrings #-}

module Database where

import qualified Models as M
import Database.SQLite.Simple
    ( Connection, close, open, query_, query, execute, Only (Only) )
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

addCharacter :: Config -> M.Character -> IO ()
addCharacter config char = withDB config $ \conn -> do
    execute conn "INSERT INTO characters (id, name, age, status, association, first_appearance, actor, affinity, gender) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
        ( M.characterId char
        , M.name char
        , M.age char
        , M.status char
        , M.association char
        , M.first_appearance char
        , M.actor char
        , M.affinity char
        , M.gender char
        )
