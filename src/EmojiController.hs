{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module EmojiController (
    selectCurrentEmojiCharacter,
    flipEmojiFinished,
    getTodayEmojiCharacter
) where

import DataBus
import Database
import Models 
import Config (defaultConfig)
import System.Random (randomRIO)
import Data.Time.Clock
import Data.Time.Calendar
import Control.Monad (when)
import Data.List (intersect)

-- Function to select an emoji-based character for the day
selectCurrentEmojiCharacter :: IO ()
selectCurrentEmojiCharacter = do
    gameStateResult <- getJSON
    case gameStateResult of
        Left err -> putStrLn $ "Error reading game state: " ++ err
        Right [] -> putStrLn "Game state is empty. Cannot select character."
        Right (gs:_) -> do
            currentDay <- utctDay <$> getCurrentTime
            let validUntilDay = ModifiedJulianDay (fromIntegral (valid_until (emoji gs))) 

            when (currentDay > validUntilDay) $ do
                allCharacters <- getCharactersWithEmoji defaultConfig
                let availableCharacters = filter (`notElem` blacklist gs) [1..length allCharacters]
                
                if null availableCharacters
                    then putStrLn "No available characters to select (all are blacklisted)."
                    else do
                        randomId <- randomRIO (1, length availableCharacters)
                        let (newYear, newMonth, newDay) = toGregorian (addDays 1 currentDay)
                            updatedEmoji = (emoji gs)
                                { today_character_id = randomId
                                , valid_until = fromIntegral $ toModifiedJulianDay $ fromGregorian newYear newMonth newDay
                                , finished = False
                                }
                            
                        let updatedBlacklist = if length (blacklist gs) >= 30
                                               then take 29 (blacklist gs) ++ [randomId]
                                               else randomId : blacklist gs
                            updatedGameState = [gs { emoji = updatedEmoji, blacklist = updatedBlacklist }]
                        writeJSON updatedGameState

-- Function to flip the finished flag for emoji mode
flipEmojiFinished :: IO ()
flipEmojiFinished = do
    gameStateResult <- getJSON
    case gameStateResult of
        Left err -> putStrLn $ "Error reading game state: " ++ err
        Right [] -> putStrLn "Game state is empty. Cannot update."
        Right (gs:_) -> do
            let updatedGameState = [gs { emoji = (emoji gs) { finished = True } }]
            writeJSON updatedGameState

-- Function to get the emoji-based character of the day
getTodayEmojiCharacter :: IO (Maybe Character)
getTodayEmojiCharacter = do
    gameStateResult <- getJSON
    case gameStateResult of
        Left err -> putStrLn err >> return Nothing
        Right [] -> putStrLn "Game state is empty." >> return Nothing
        Right (gs:_) -> getCharacterById defaultConfig (today_character_id $ emoji gs)
