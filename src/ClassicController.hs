{-# LANGUAGE OverloadedStrings #-}

module ClassicController (
    selectCurrentCharacter,
    flipFinished,
    getTodayCharacter,
    MatchResult(..),
    compareCharacters
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
import Data.Map as Map

-- Function to select a character for the day
selectCurrentCharacter :: IO ()
selectCurrentCharacter = do
    gameStateResult <- getJSON
    case gameStateResult of
        Left err -> putStrLn $ "Error reading game state: " ++ err
        Right [] -> putStrLn "Game state is empty. Cannot select character."
        Right (gs:_) -> do
            currentDay <- utctDay <$> getCurrentTime
            let validUntilDay = ModifiedJulianDay (fromIntegral (valid_until (classic gs))) 

            -- Ensure that a character is selected only if the game state is valid for today
            when (currentDay > validUntilDay) $ do
                allCharacters <- getAllCharacters defaultConfig
                let availableCharacters = filter (`notElem` blacklist gs) [1..length allCharacters]
                
                if null availableCharacters
                    then putStrLn "No available characters to select (all are blacklisted)."
                    else do
                        randomId <- randomRIO (1, length availableCharacters)
                        let (newYear, newMonth, newDay) = toGregorian (addDays 1 currentDay)
                            updatedClassic = (classic gs)
                                { today_character_id = randomId
                                , valid_until = fromIntegral $ toModifiedJulianDay $ fromGregorian newYear newMonth newDay
                                , finished = False
                                }
                            
                            -- Update the blacklist, ensuring the size is 30
                        let updatedBlacklist = if length (blacklist gs) >= 30
                                               then take 29 (blacklist gs) ++ [randomId]  -- Remove oldest and add new one at the front
                                               else randomId : blacklist gs
                            updatedGameState = [gs { classic = updatedClassic, blacklist = updatedBlacklist }]
                        writeJSON updatedGameState

-- Function to flip the finished flag
flipFinished :: IO ()
flipFinished = do
    gameStateResult <- getJSON
    case gameStateResult of
        Left err -> putStrLn $ "Error reading game state: " ++ err
        Right [] -> putStrLn "Game state is empty. Cannot update."
        Right (gs:_) -> do
            let updatedGameState = [gs { classic = (classic gs) { finished = True } }]
            writeJSON updatedGameState

-- Function to get the character of the day
getTodayCharacter :: IO (Maybe Character)
getTodayCharacter = do
    gameStateResult <- getJSON
    case gameStateResult of
        Left err -> putStrLn err >> return Nothing
        Right [] -> putStrLn "Game state is empty." >> return Nothing
        Right (gs:_) -> getCharacterById defaultConfig (today_character_id $ classic gs)

-- Helper function to compare two characters
data MatchResult = Correct | Partial | Wrong deriving (Eq, Show)

temporadas :: Map.Map Int String
temporadas = Map.fromList
    [ (1, "Iniciação")
    , (2, "O segredo na Floresta")
    , (3, "Desconjuração")
    , (4, "Calamidade")
    , (5, "O segredo na Ilha")
    , (6, "Sinais do Outro Lado")
    , (7, "Quarentena")
    , (8, "Enigma do Medo")
    , (9, "Natal Macabro")
    ]

compareCharacters :: Character -> Character -> [(String, MatchResult)]
compareCharacters secret guess = 
    [ ("Name", checkEquality (name secret) (name guess))
    , ("Age", checkEquality (age secret) (age guess))
    , ("Status", checkEquality (status secret) (status guess))
    , ("Association", compareStrings (association secret) (association guess))
    , ("First Appearance", checkEquality (first_appearance secret) (first_appearance guess))
    , ("Actor", checkEquality (actor secret) (actor guess))
    , ("Affinity", compareStrings (affinity secret) (affinity guess))
    , ("Gender", checkEquality (gender secret) (gender guess))
    ]

checkEquality :: Eq a => a -> a -> MatchResult
checkEquality x y = if x == y then Correct else Wrong

compareStrings :: String -> String -> MatchResult
compareStrings s1 s2
    | s1 == s2  = Correct
    | not (null common) = Partial
    | otherwise = Wrong
  where common = words s1 `intersect` words s2
