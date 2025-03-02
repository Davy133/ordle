import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as B
import Control.Monad (when)
import qualified Data.Set as Set
import Config

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
            monster <- getDailyMonster config
            (classicCharacter quoteCharacter emojiCharacter) = getDailyCharacters config

        Left err -> do
            return [-1]

getDailyMonster :: Config -> Queues -> M.Monster
getDailyMonster config q= do
    dailyMonster <- getRandomMonster config
     if checkRepetitions (monsters q) dailyMonster
        then getDailyMonster config
        else return dailyMonster

getDailyCharacter :: Config -> Queues -> (Queues -> [String]) -> M.Character
getDailyCharacter config q queueName= do
     dailyCharacter <- getRandomCharacter config
     if checkRepetitions (queueName q) dailyCharacter
        then getDailyCharacter config q queueName
        else return dailyCharacter

getDailyCharacters :: Config -> IO (M.Character, M.Character, M.Character)
getDailyCharacters config = do
    c1 <- getDailyCharacter config classic
    c2 <- getDailyCharacter config quote
    c3 <- getDailyCharacter config emoji
    checkUnicity config c1 c2 c3

checkUnicity :: Config -> M.Character -> M.Character -> M.Character -> IO (M.Character, M.Character, M.Character)
checkUnicity config c1 c2 c3
    | Set.size (Set.fromList [c1, c2, c3]) == 3 = return (c1, c2, c3)
    | otherwise = do
        let (newC1, newC2, newC3) = 
            if c1 == c2 || c1 == c3 then do
                newCharacter = getRandomCharacter config "classic"
                (newCharacter, c2, c3) 
            else do
                c2 <- getDailyCharacter config "quote"
                (c1, newCharacter, c3)
        checkUnicity config newC1 newC2 newC3

  