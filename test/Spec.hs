{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

import Test.Hspec
import qualified Data.Set as Set
import qualified Models as M
import Config (loadConfig, defaultConfig, Config(..))
import Database (getMonsters, getRandomMonster, getCharacters, getRandomCharacter)
import JsonParser (setDaily)
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
    
  describe "JsonParser Module" $ do
    it "setdaily test" $ do
      a <- setDaily "control_queues.json"
      print(a)


  