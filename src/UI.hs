module UI where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import Database ( getRandomCharacter, getAllCharacters, getCharacterById )
import Config ( loadConfig)
import Models
import GameLogic

start :: IO ()
start = startGUI defaultConfig setup

extractNameAndId :: [Character] -> [(String, Int)]
extractNameAndId characters = map (\c -> (name c, characterId c)) characters


setup :: Window -> UI ()
setup window = do
    config <- liftIO $ loadConfig "config.dhall"
    randomCharacter <- liftIO $ getRandomCharacter config
   
    case randomCharacter of
        Just c -> liftIO $ putStrLn $ "Random character: " ++ show c
        Nothing -> liftIO $ putStrLn "No character found"
    
    return window # set title "Ordle"

    button <- UI.button #+ [string "Guess"]
    getBody window #+ [element button]

    characters <- liftIO $ getAllCharacters config
    let namesAndIds = extractNameAndId characters
    dropdown <- UI.select #+ map (\(name, id) -> UI.option # set value (show id) #+ [string name]) namesAndIds

    getBody window #+ [element dropdown]
    on UI.click button $ \_ -> do
        selectedValue <- get value dropdown
        selectedCharacter <- liftIO $ getCharacterById config (read selectedValue)

        case selectedCharacter of
            Just c -> do
                liftIO $ putStrLn $ "Selected character: " ++ show c
                case randomCharacter of
                    Just r -> do
                        let results = compareCharacters r c  -- Compare the two Characters
                        liftIO $ putStrLn $ "Results: " ++ show results
                    Nothing -> liftIO $ putStrLn "No random character found"
            Nothing -> liftIO $ putStrLn "No character found"
        -- compareCharacters selectedCharacter randomCharacter
        

        -- liftIO $ putStrLn $ "Selected value: " ++ selectedValue

        -- element button # set text "Clicked!"