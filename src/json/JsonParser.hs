{-# LANGUAGE DeriveGeneric #-}

module JsonParser(parseQueues, checkRepetitions, setDaily, getDailyCharacter, getDailyCharacters, getDailyMonster, checkUniqueness) where
  
import Data.Aeson
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as B
import Control.Monad ()
import qualified Data.Set as Set
import Config (loadConfig, Config(..))
import Database (getRandomMonster, getRandomCharacter)
import Models (name,monsterName, monsterId, characterId, quote)
import qualified Models as M


newtype ControlQueues = ControlQueues
  { queues :: Queues
  } deriving (Show, Generic)
data Queues = Queues
  { classic :: [String],
    quotes :: [String],
    emoji :: [String],
    monsters :: [String]
  } deriving (Show, Generic)

instance FromJSON Queues
instance ToJSON Queues

parseQueues :: FilePath -> IO (Either String Queues)
parseQueues jsonFile = do
  jsonData <- B.readFile jsonFile
  return (eitherDecode jsonData :: Either String Queues)

checkRepetitions :: [String] -> String -> Bool
checkRepetitions [] _ = False
checkRepetitions (h:t) e
    |h==e = True
    |otherwise = checkRepetitions t e

setDaily :: FilePath -> IO [Integer]
setDaily jsonFile = do
    result <- parseQueues jsonFile
    case result of
        Right q -> do
            loadedConfig <- loadConfig "config.dhall"
            (classicCharacter, quoteCharacter, emojiCharacter) <- getDailyCharacters loadedConfig
            monster <- getDailyMonster loadedConfig q
            
            let updatedQueues = q { classic = name classicCharacter : classic q
                                  , quotes = name quoteCharacter : quotes q
                                  , emoji = name emojiCharacter : emoji q
                                  , monsters = monsterName monster : monsters q
                                  }
            B.writeFile jsonFile (encode updatedQueues)
            return [toInteger (monsterId monster), toInteger (characterId classicCharacter), toInteger (characterId quoteCharacter), toInteger (characterId emojiCharacter)]

        Left _ -> do
            return [-1]

getDailyMonster :: Config -> Queues -> IO M.Monster
getDailyMonster config q = do
    dailyMonster <- getRandomMonster config
    if checkRepetitions (monsters q) (monsterName dailyMonster)
    then getDailyMonster config q  
    else return dailyMonster

getDailyCharacter :: Config -> Queues -> (Queues -> [String]) -> IO M.Character
getDailyCharacter config q queueName = do
    dailyCharacter <- getRandomCharacter config
    if checkRepetitions (queueName q) (name dailyCharacter)
    then getDailyCharacter config q queueName
    else return dailyCharacter

getDailyCharacters :: Config -> IO (M.Character, M.Character, M.Character)
getDailyCharacters config = do
    result <- parseQueues "control_queues.json" -- Add the correct path to your JSON file
    case result of
        Right q -> do
            c1 <- getDailyCharacter config q classic
            c2 <- getDailyCharacter config q quotes
            c3 <- getDailyCharacter config q emoji
            checkUniqueness config q c1 c2 c3
        Left _ -> error "Failed to parse queues"

checkUniqueness :: Config -> Queues -> M.Character -> M.Character -> M.Character -> IO (M.Character, M.Character, M.Character)
checkUniqueness config q c1 c2 c3 = do
    if Set.size (Set.fromList [c1, c2, c3]) == 3
    then return (c1, c2, c3) 
    else do
        if c1 == c2 || c1 == c3 then do
            newCharacter <- getDailyCharacter config q classic
            return (newCharacter, c2, c3)
        else do
            newCharacter <- getDailyCharacter config q quotes
            return (c1, newCharacter, c3)

  