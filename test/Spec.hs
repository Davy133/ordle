{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import qualified Data.Set as Set
import qualified Models as M
import Config (loadConfig, defaultConfig, Config(..))
import Database (getMonsters, getRandomMonster, getCharacters, getRandomCharacter)
import JsonParser (parseQueues, checkRepetitions, setDaily, getDailyCharacter, getDailyCharacters, getDailyMonster, checkUniqueness)
import Data.Either (isRight)

main :: IO ()
main = hspec $ do
  describe "Config Module" $ do
    it "Loads configuration from a valid Dhall file" $ do
      config <- loadConfig "config.dhall"
      config `shouldBe` Config { staticDir = "assets", dbPath = "mydatabase.db" }

    it "Has a correct default configuration" $ do
      defaultConfig `shouldBe` Config { staticDir = "static", dbPath = "default.db" }

  describe "Database Module" $ do
    it "Gets all monsters" $ do
      config <- loadConfig "config.dhall"
      monsters <- getMonsters config
      length monsters `shouldNotBe` 0
      -- print monsters
    it "Gets a random monsters" $ do
      config <- loadConfig "config.dhall"
      monster <- getRandomMonster config
      print monster
    it "Gets all characters" $ do
      config <- loadConfig "config.dhall"
      characters <- getCharacters config
      length characters `shouldNotBe` 0
      -- print characters
    it "Gets a random character" $ do
      config <- loadConfig "config.dhall"
      character <- getRandomCharacter config
      print character
    
  -- Tests for parseQueues function
  describe "parseQueues" $ do
    it "Parses a valid JSON file correctly" $ do
      -- Mock a valid JSON file or test with a real file
      result <- parseQueues "valid_queues.json"
      result `shouldSatisfy` isRight  -- Assuming Either is a result (Right means success)

    it "Returns error for invalid JSON format" $ do
      result <- parseQueues "invalid_queues.json"
      result `shouldBe` Left "Error parsing file"

  -- Tests for checkRepetitions function
  describe "checkRepetitions" $ do
    it "Returns False for an empty list" $ do
      checkRepetitions [] "test" `shouldBe` False

    it "Returns True if the element is the first in the list" $ do
      checkRepetitions ["test", "another"] "test" `shouldBe` True

    it "Returns True if the element is in the list" $ do
      checkRepetitions ["another", "test"] "test" `shouldBe` True

    it "Returns False if the element is not in the list" $ do
      checkRepetitions ["another"] "test" `shouldBe` False

  -- Tests for setDaily function
  describe "setDaily" $ do
    it "Successfully updates queues with new characters and monsters" $ do
      config <- loadConfig "config.dhall"
      result <- setDaily "queues.json" config
      result `shouldSatisfy` (\r -> length r == 4)  -- Should return a list of 4 IDs

  -- Tests for getDailyMonster function
  describe "getDailyMonster" $ do
    it "Ensures the monster is not repeated in the queue" $ do
      config <- loadConfig "config.dhall"
      queues <- parseQueues "queues.json"
      case queues of
        Right q -> do
          monster <- getDailyMonster config q
          checkRepetitions (monsters q) (monsterName monster) `shouldBe` False
        Left _ -> error "Failed to load queues"

  -- Tests for getDailyCharacter function
  describe "getDailyCharacter" $ do
    it "Ensures the character is not repeated in the queue" $ do
      config <- loadConfig "config.dhall"
      queues <- parseQueues "queues.json"
      case queues of
        Right q -> do
          character <- getDailyCharacter config q classic
          checkRepetitions (classic q) character `shouldBe` False
        Left _ -> error "Failed to load queues"

  -- Tests for getDailyCharacters function
  describe "getDailyCharacters" $ do
    it "Ensures all 3 characters are unique" $ do
      config <- loadConfig "config.dhall"
      (c1, c2, c3) <- getDailyCharacters config
      Set.size (Set.fromList [c1, c2, c3]) `shouldBe` 3
  -- Tests for checkUniqueness function
  describe "checkUniqueness" $ do
    it "Ensures characters are unique and reselects if necessary" $ do
      config <- loadConfig "config.dhall"
      -- Mock or create a test case where two characters are the same
      (newC1, newC2, newC3) <- checkUniqueness config c1 c2 c3
      (newC1, newC2, newC3) <- checkUnicity config c1 c2 c3
      Set.size (Set.fromList [newC1, newC2, newC3]) `shouldBe` 3


