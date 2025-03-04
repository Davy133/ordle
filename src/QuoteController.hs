
module QuoteController (
    selectCurrentQuoteCharacter,
    flipQuoteFinished,
    getTodayQuoteCharacter
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

-- Function to select a quotes-based character for the day
selectCurrentQuoteCharacter :: IO ()
selectCurrentQuoteCharacter = do
    gameStateResult <- getJSON
    case gameStateResult of
        Left err -> putStrLn $ "Error reading game state: " ++ err
        Right [] -> putStrLn "Game state is empty. Cannot select character."
        Right (gs:_) -> do
            currentDay <- utctDay <$> getCurrentTime
            let validUntilDay = ModifiedJulianDay (fromIntegral (valid_until (quotes gs))) 

            when (currentDay > validUntilDay) $ do
                allCharacters <- getCharactersWithQuotes defaultConfig
                let availableCharacters = filter (`notElem` blacklist gs) [1..length allCharacters]
                
                if null availableCharacters
                    then putStrLn "No available characters to select (all are blacklisted)."
                    else do
                        randomId <- randomRIO (1, length availableCharacters)
                        let (newYear, newMonth, newDay) = toGregorian (addDays 1 currentDay)
                            updatedQuote = (quotes gs)
                                { today_character_id = randomId
                                , valid_until = fromIntegral $ toModifiedJulianDay $ fromGregorian newYear newMonth newDay
                                , finished = False
                                }
                            
                        let updatedBlacklist = if length (blacklist gs) >= 30
                                               then take 29 (blacklist gs) ++ [randomId]
                                               else randomId : blacklist gs
                            updatedGameState = [gs { quotes = updatedQuote, blacklist = updatedBlacklist }]
                        writeJSON updatedGameState

-- Function to flip the finished flag for quotes mode
flipQuoteFinished :: IO ()
flipQuoteFinished = do
    gameStateResult <- getJSON
    case gameStateResult of
        Left err -> putStrLn $ "Error reading game state: " ++ err
        Right [] -> putStrLn "Game state is empty. Cannot update."
        Right (gs:_) -> do
            let updatedGameState = [gs { quotes = (quotes gs) { finished = True } }]
            writeJSON updatedGameState

-- Function to get the quotes-based character of the day
getTodayQuoteCharacter :: IO (Maybe Character)
getTodayQuoteCharacter = do
    gameStateResult <- getJSON
    case gameStateResult of
        Left err -> putStrLn err >> return Nothing
        Right [] -> putStrLn "Game state is empty." >> return Nothing
        Right (gs:_) -> getCharacterById defaultConfig (today_character_id $ quotes gs)
