module ClassicController where

import qualified DataBus as DB
import Database
import Models
import System.Random (randomRIO)
import Data.Aeson (decode, encode, eitherDecode)
import qualified Data.ByteString.Lazy as B

import Config (defaultConfig)
import Data.Time.Clock
import Data.Time.Calendar
import qualified Control.Monad
import Data.List (intersect)

jsonFile :: FilePath
jsonFile = "game_state.json" -- Fix this later, put it in the config file

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

writeJSON :: B.ByteString -> IO ()
writeJSON = B.writeFile jsonFile

selectCurrentCharacter :: IO ()
selectCurrentCharacter = do
    gameState <- (eitherDecode <$> getJSON) :: IO (Either String [DB.GameState])
    case gameState of
        Left err -> putStrLn err
        Right ps -> do
            currentTime <- getCurrentTime
            let currentDay = utctDay currentTime
            let validUntil = DB.valid_until . DB.classic . head $ ps
            let validUntilDay = addDays (fromIntegral validUntil) (fromGregorian 1970 1 1)
            Control.Monad.when (currentDay > validUntilDay) $ do
                allCharacters <- getAllCharacters defaultConfig
                let characterCount = length allCharacters
                randomId <- randomRIO (1, characterCount)
                let newValidUntil = addUTCTime (24 * 60 * 60 :: NominalDiffTime) currentTime 
                let updatedGameState = map (\p -> p { DB.classic = (DB.classic p) { DB.today_character_id = randomId, DB.valid_until = fromIntegral . toModifiedJulianDay $ utctDay newValidUntil, DB.finished = False } }) ps
                writeJSON (encode updatedGameState) 

flipFinished :: IO ()
flipFinished = do
    gameState <- (eitherDecode <$> getJSON) :: IO (Either String [DB.GameState])
    case gameState of
        Left err -> putStrLn err
        Right ps -> do
            let updatedGameState = map (\p -> p { DB.classic = (DB.classic p) { DB.finished = True } }) ps
            writeJSON (encode updatedGameState)


getTodayCharacter :: IO (Maybe Character)
getTodayCharacter = do
    gameState <- (eitherDecode <$> getJSON) :: IO (Either String [DB.GameState])
    case gameState of
        Left err -> putStrLn err >> return Nothing
        Right ps -> do
            let todayCharacterId = DB.today_character_id . DB.classic . head $ ps
            character <- getCharacterById defaultConfig todayCharacterId
            return character


data MatchResult = Correct | Partial | Wrong deriving (Eq, Show)

compareCharacters :: Character -> Character -> [(String, MatchResult)]
compareCharacters secret guess = 
    [ ("Name", if name secret == name guess then Correct else Wrong)
    , ("Age", if age secret == age guess then Correct else Wrong)
    , ("Status", if status secret == status guess then Correct else Wrong)
    , ("Association", compareStrings (association secret) (association guess))
    , ("First Appearance", if first_appearance secret == first_appearance guess then Correct else Wrong)
    , ("Actor", if actor secret == actor guess then Correct else Wrong)
    , ("Affinity", compareStrings (affinity secret) (affinity guess))
    , ("Gender", if gender secret == gender guess then Correct else Wrong)
    ]
    
compareStrings :: String -> String -> MatchResult
compareStrings s1 s2
    | s1 == s2           = Correct
    | not (null common)  = Partial
    | otherwise          = Wrong
  where common = words s1 `intersect` words s2 
