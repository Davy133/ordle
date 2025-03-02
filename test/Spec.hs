{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Config (loadConfig, defaultConfig, Config(..))
import Database (getMonsters, getRandomMonster)

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
