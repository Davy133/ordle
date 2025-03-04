{-# LANGUAGE OverloadedStrings #-}

module MonsterController (
    selectCurrentMonster,
    getTodayMonster
) where

import DataBus
import Database
import Models
import Config (defaultConfig)
import System.Random (randomRIO)
import Data.Time.Clock
import Data.Time.Calendar
import Control.Monad (when)

-- Function to select a monster for the day
selectCurrentMonster :: IO ()
selectCurrentMonster = do
    gameStateResult <- getJSON
    case gameStateResult of
        Left err -> putStrLn $ "Error reading game state: " ++ err
        Right [] -> putStrLn "Game state is empty. Cannot select monster."
        Right (gs:_) -> do
            currentDay <- utctDay <$> getCurrentTime
            let validUntilDay = ModifiedJulianDay (fromIntegral (valid_until (monsters gs)))
            
            when (currentDay > validUntilDay) $ do
                allMonsters <- getAllMonsters defaultConfig
                randomIndex <- randomRIO (0, length allMonsters - 1)
                let selectedMonster = allMonsters !! randomIndex
                    (newYear, newMonth, newDay) = toGregorian (addDays 1 currentDay)
                    updatedMonster = (monsters gs)
                        { today_character_id = monsterId selectedMonster
                        , valid_until = fromIntegral $ toModifiedJulianDay $ fromGregorian newYear newMonth newDay
                        }
                    updatedGameState = [gs { monsters = updatedMonster }]
                writeJSON updatedGameState

-- Function to get the monster of the day
getTodayMonster :: IO (Maybe Monster)
getTodayMonster = do
    gameStateResult <- getJSON
    case gameStateResult of
        Left err -> putStrLn err >> return Nothing
        Right [] -> putStrLn "Game state is empty." >> return Nothing
        Right (gs:_) -> getMonsterById defaultConfig (today_character_id $ monsters gs)
