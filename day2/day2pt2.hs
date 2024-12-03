import Prelude

validDiffs :: [Int]
validDiffs = [-3, -2, -1, 1, 2, 3]

removeFromList :: Int -> [Int] -> [Int]
removeFromList _ [] = []
removeFromList 0 (n:ns) = ns
removeFromList idx (n:ns) = n : (removeFromList (idx - 1) ns)

-- Check whether report is valid with each element removed
checkDampener :: Int -> [Int] -> Bool
checkDampener (-1) _ = False
checkDampener idx ns = (checkReportDiffs $ findDiffs (removeFromList idx ns)) || (checkDampener (idx - 1) ns)

-- Check if diffs are safe
checkReportDiffs :: [Int] -> Bool
checkReportDiffs ns = ((all (<0) ns) || (all (>0) ns)) && (all (`elem` validDiffs) ns)

findDiffs :: [Int] -> [Int]
findDiffs ns = zipWith (-) ns (tail ns)

checkReportValid :: [Int] -> Bool
checkReportValid ns = checkReportDiffs (findDiffs ns) || checkDampener (length ns) ns

parseInput :: [String] -> [[Int]]
parseInput [] = []
parseInput (x:xs) = (map read (words x)) : parseInput xs

main :: IO ()
main = do
    input <- readFile "input.txt"
    let reports = parseInput $ lines input
    let reportValidities = map checkReportValid reports
    let numValidReports = length $ filter (==True) reportValidities
    putStrLn $ show numValidReports
