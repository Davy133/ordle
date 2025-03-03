import ClassicController


main :: IO ()
main = do
    getTodayCharacter >>= print