{-# LANGUAGE OverloadedStrings #-}

import Graphics.UI.Threepenny.Core
import Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Elements
import Graphics.UI.Threepenny.Events
import qualified Graphics.UI.Threepenny.Attributes as Attr
import Control.Monad (void)
import ClassicController
import Database (getCharacterById)
import Config (loadConfig)
import Models (Character(..))
import System.IO (hSetEncoding, stdout, utf8) 
import Data.List (isInfixOf)
import Data.List.Split (splitOn)

main :: IO ()
main = do
    hSetEncoding stdout utf8 
    startGUI defaultConfig { jsPort = Just 8023 } setup

setup :: Window -> UI ()
setup window = do
    liftIO selectCurrentCharacter
    todayCharacter <- liftIO getTodayCharacter
    case todayCharacter of
        Just char -> void $ getBody window #+ [string $ "Today's character is: " ++ Models.name char]
        Nothing -> void $ getBody window #+ [string "Today's character is not available"]
    return window # set title "Character Guessing Game"

    guessInput <- input # set (UI.attr "placeholder") "Guess the character"
    guessButton <- button # set text "Guess"
    hintButton <- button # set text "Hint"

    infoTable <- table #+ [UI.tr #+ Prelude.map (th #. "header" #+) (Prelude.map (return . string) ["Name", "Age", "Gender", "Affiliation", "Actor", "First Appearance"])
                           , UI.body]

    on UI.click guessButton $ \_ -> do
        guess <- get value guessInput
        let guessId = read guess :: Int
        config <- liftIO $ loadConfig "config.dhall"
        character <- liftIO $ getCharacterById config guessId
        todayCharacter <- liftIO getTodayCharacter
        case (character, todayCharacter) of
            (Just char, Just todayChar) -> do
                let compareField field
                      | field char == field todayChar = (string (field char), "green")
                      | field char == field todayChar = (string (field char), "green")
                      | any (`isInfixOf` field char) (splitOn "," (field todayChar)) || any (`isInfixOf` field todayChar) (splitOn "," (field char)) = (string (field char), "DarkGoldenRod")
                      | otherwise = (string (field char), "red")
                    details = [ compareField Models.name
                              , compareField (show . age)
                              , compareField gender
                              , compareField association
                              , compareField actor
                              , compareField first_appearance
                              ]
                void $ element infoTable #+ [UI.tr #+ Prelude.map (\(content, color) -> td # set style [("color", color)] #+ [content]) details]
            (Nothing, _) -> do
                void $ element infoTable #+ [UI.tr #+ [td #+ [string "Character not found"]]]
            (_, Nothing) -> do
                void $ element infoTable #+ [UI.tr #+ [td #+ [string "Today's character is not available"]]]
        return ()

    on UI.click hintButton $ \_ -> do
        todayCharacter <- liftIO getTodayCharacter
        case todayCharacter of
            Just char -> void $ element infoTable #+ [UI.tr #+ [td #+ [string $ "Hint: " ++ Models.status char]]]
            Nothing -> void $ element infoTable #+ [UI.tr #+ [td #+ [string "Today's character is not available"]]]
        return ()


    -- Container div to center content
    container <- UI.div # set style [("display", "flex"), ("justify-content", "center"), ("align-items", "center"), ("height", "100vh"), ("flex-direction", "column")]

    getBody window #+ [element container #+ [element guessInput, element guessButton, element hintButton, element infoTable]]
    return ()
