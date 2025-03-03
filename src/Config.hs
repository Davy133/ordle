{-# LANGUAGE DeriveGeneric #-}

module Config(Config(..), loadConfig, defaultConfig) where
import Dhall

data Config = Config
  { 
    staticDir :: FilePath,
    dbPath :: FilePath
  } deriving (Eq, Show, Generic)

instance FromDhall Config

loadConfig :: FilePath -> IO Config
loadConfig path = inputFile auto path

defaultConfig :: Config 
defaultConfig = Config {
    staticDir = "static",
    dbPath = "characters.db"
}
