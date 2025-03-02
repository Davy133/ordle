checkRepetitions :: [String] -> String -> Bool
checkRepetitions [] _ = False
checkRepetitions (h:t) e
    |h==e = True
    |otherwise = checkRepetitions t e