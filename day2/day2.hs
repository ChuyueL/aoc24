import Prelude

validDiffs :: [Int]
validDiffs = [-3, -2, -1, 1, 2, 3]

checkReportDiffs :: [Int] -> Bool
checkReportDiffs ns = ((all (<0) ns) || (all (>0) ns)) && (all (`elem` validDiffs) ns)

findDiffs :: [Int] -> [Int]
findDiffs ns = zipWith (-) ns (tail ns)

checkDiffsValid :: [[Int]] -> [Bool]
checkDiffsValid [] = []
checkDiffsValid (ns:nss) = (checkReportDiffs ns) : checkDiffsValid nss

parseInput :: [String] -> [[Int]]
parseInput [] = []
parseInput (x:xs) = (map read (words x)) : parseInput xs

main :: IO ()
main = do
    input <- readFile "input.txt"
    let reports = parseInput $ lines input
    let diffs = map findDiffs reports
    let numValidDiffs = length $ filter (==True) (checkDiffsValid diffs)
    putStrLn $ show numValidDiffs
