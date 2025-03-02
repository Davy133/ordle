{-# LANGUAGE OverloadedStrings #-}

module GameLogic where

import Models (Character(..))
import Data.List (intersect)

data MatchResult = Correct | Partial | Wrong deriving (Eq, Show)

compareCharacters :: Character -> Character -> [(String, MatchResult)]
compareCharacters secret guess = 
    [ ("Name", if name secret == name guess then Correct else Wrong)
    , ("Age", if age secret == age guess then Correct else Partial)
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

loseLife :: Int -> Int
loseLife lives = max 0 (lives - 1)

checkGameStatus :: [(String, MatchResult)] -> Int -> Bool
checkGameStatus results lives = 
    all (\(_, r) -> r == Correct) results || lives <= 0
