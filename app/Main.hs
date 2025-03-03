import ClassicController
import Database
import Config

main :: IO ()
main = do
    selectCurrentCharacter
    randomChar <- getCharacterById defaultConfig 43
    currentChar <- getTodayCharacter
    case (randomChar, currentChar) of
        (Just r, Just c) -> do
            let result = compareCharacters r c
            print result
        _ -> putStrLn "Error: Could not retrieve characters"