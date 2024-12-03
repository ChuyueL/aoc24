import Prelude
import Data.List

absDiff :: Int -> Int -> Int
absDiff a b = abs (a - b)

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
    let output = sum $ zipWith absDiff firsts seconds
    putStrLn (show output)
