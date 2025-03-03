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
