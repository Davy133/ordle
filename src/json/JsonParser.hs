{-# LANGUAGE DeriveGeneric #-}

module JsonParser(parseQueues, checkRepetitions, setDaily, getDailyCharacter, getDailyCharacters, getDailyMonster, checkUnicity) where
  
import Data.Aeson
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as B
import Control.Monad (when)
import qualified Data.Set as Set
import Config (loadConfig, defaultConfig, Config(..))
import Database (getMonsters, getRandomMonster, getCharacters, getRandomCharacter)
import Models (name,monsterName, monsterId, characterId, quote)
import qualified Models as M


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

setDaily :: FilePath -> Config -> IO [Integer]
setDaily jsonFile config = do
    result <- parseQueues jsonFile
    case result of
        Right q -> do
            config <- loadConfig "config.dhall"
            (classicCharacter, quoteCharacter, emojiCharacter) <- getDailyCharacters config
            monster <- getDailyMonster config q
            
            let updatedQueues = q { classic = name classicCharacter : classic q
                                  , quotes = name quoteCharacter : quotes q
                                  , emoji = name emojiCharacter : emoji q
                                  , monsters = monsterName monster : monsters q
                                  }
            B.writeFile jsonFile (encode updatedQueues)
            return [toInteger (monsterId monster), toInteger (characterId classicCharacter), toInteger (characterId quoteCharacter), toInteger (characterId emojiCharacter)]

        Left err -> do
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
    c1 <- getDailyCharacter config "classic" classic
    c2 <- getDailyCharacter config "quote" quote
    c3 <- getDailyCharacter config "emoji" emoji
    checkUnicity config c1 c2 c3

checkUnicity :: Config -> M.Character -> M.Character -> M.Character -> IO (M.Character, M.Character, M.Character)
checkUnicity config c1 c2 c3 = do
    if Set.size (Set.fromList [c1, c2, c3]) == 3
    then return (c1, c2, c3) 
    else do
        if c1 == c2 || c1 == c3 then do
            newCharacter <- getDailyCharacter config "classic"
            return (newCharacter, c2, c3)
        else do
            newCharacter <- getDailyCharacter config "quote"
            return (c1, newCharacter, c3)

  