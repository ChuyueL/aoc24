import Prelude

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

findMiddleElem :: [Int] -> Int
findMiddleElem xs = xs!!midIdx
    where
        midIdx = (length xs `div` 2)

updateValidForRule :: [Int] -> (Int, Int) -> Bool
updateValidForRule u (f,s)
    | f `elem` u && s `elem` u = filter (\n -> n==f || n==s) u == [f,s]
    | otherwise = True

checkUpdate :: [Int] -> [(Int, Int)] -> Bool
checkUpdate u [] = True
checkUpdate u (r:rs) = updateValidForRule u r && checkUpdate u rs 

parseRule :: String -> (Int, Int)
parseRule s = (read $ head pages, read $ last pages)
    where
        pages = wordsWhen (=='|') s

parseUpdate :: String -> [Int]
parseUpdate = map read . wordsWhen (==',')

-- returns (rules, updates)
parseInputLines :: [String] -> ([(Int,Int)], [[Int]])
parseInputLines [] = ([], [])
parseInputLines (x:xs)
    | '|' `elem` x = ((parseRule x) : (fst $ parseInputLines xs), snd $ parseInputLines xs)
    | ',' `elem` x = (fst $ parseInputLines xs, (parseUpdate x): (snd $ parseInputLines xs))
    | otherwise = parseInputLines xs

main :: IO ()
main = do
    input <- readFile "input.txt"
    let (rules, updates) = parseInputLines $ lines input
    print $ "Rules" ++ show rules
    print $ "Updates" ++ show updates
    let validUpdates = filter (\u -> (checkUpdate u rules == True)) updates
    print $ "Valid updates " ++ show validUpdates
    let total = sum $ map findMiddleElem validUpdates
    print total

