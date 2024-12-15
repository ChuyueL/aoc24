import Prelude

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

removeElemFromList :: Int -> [Int] -> [Int]
removeElemFromList _ [] = []
removeElemFromList x (y:ys)
    | x == y = removeElemFromList x ys
    | otherwise = y : removeElemFromList x ys

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

-- update is in wrong order for this rule so snd must occur before fst
fixUpdateForRule :: [Int] -> (Int,Int) -> [Int]
fixUpdateForRule [] _ = []
fixUpdateForRule (x:xs) r@(f,s)
    | x == s = [f,s] ++ removeElemFromList f xs
    | otherwise = x : fixUpdateForRule xs r

fixUpdate :: [Int] -> [(Int, Int)] -> [Int]
fixUpdate u [] = u
fixUpdate u (r:rs)
    | updateValidForRule u r == False = fixUpdate (fixUpdateForRule u r) rs
    | otherwise = fixUpdate u rs

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
    let invalidUpdates = filter (\u -> (checkUpdate u rules == False)) updates
    let fixedUpdates = map (\u -> fixUpdate u rules) invalidUpdates
    -- running the fixing algo the second time catches any cases where fixing one rule might
    -- make an update no longer valid for a rule that had previously been fixed (yes this is janky)
    let fixedUpdates1 = map (\u -> fixUpdate u rules) fixedUpdates 
    print $ "Fixed updates " ++ show fixedUpdates1
    let total = sum $ map findMiddleElem fixedUpdates1
    print total
