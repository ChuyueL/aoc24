import Prelude
import Data.List

numAppearances :: Int -> [Int] -> Int
numAppearances n ns = length $ filter (==n) ns 

calcSimilarity :: [Int] -> [Int] -> Int
calcSimilarity [] _ = 0
calcSimilarity (n:ns) ms = (n * numAppearances n ms) + calcSimilarity ns ms

parseInputLine :: String -> (Int, Int)
parseInputLine s = (read $ head wordsS, read $ last wordsS)
    where wordsS = words s

parseInputLines :: [String] -> [(Int, Int)]
parseInputLines [] = []
parseInputLines (x:xs) = (parseInputLine x) : parseInputLines xs

main :: IO ()
main = do
    input <- readFile "input.txt"
    let inputLines = lines input
    let inputIntTuples = parseInputLines inputLines
    let firsts = sort $ map fst inputIntTuples
    let seconds = sort $ map snd inputIntTuples
    let output = calcSimilarity firsts seconds
    putStrLn (show output)
