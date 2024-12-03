import Prelude
import Data.String
import Data.List
import Data.Maybe
import Text.Read

keepUntil :: (a -> Bool) -> [a] -> [a]
keepUntil p (x:xs)
    | (p x) = []
    | otherwise = x : keepUntil p xs

getDoStr :: String -> String
getDoStr [] = []
getDoStr (c:cs)
    | (checkIfStartsWith "don't()" (c:cs)) || (checkIfStartsWith "do()" (c:cs)) = []
    | otherwise = c : getDoStr cs

checkIfStartsWith :: String -> String -> Bool
checkIfStartsWith search = isPrefixOf search

getPairs :: [Maybe Int] -> [Maybe Int] -> [(Maybe Int, Maybe Int)]
getPairs [] _ = []
getPairs _ [] = []
getPairs (a:as) (b:bs) = (a, b) : getPairs as bs

mulNums :: (Maybe Int, Maybe Int) -> Maybe Int
mulNums ((Just a),(Just b)) = Just (a * b)
mulNums (_,_)  = Nothing

getFirstSegment :: String -> String
getFirstSegment [] = []
getFirstSegment (c:cs)
    | (checkIfStartsWith "don't()" (c:cs)) || (checkIfStartsWith "do()" (c:cs)) = []
    | otherwise = c : getFirstSegment cs

main :: IO ()
main = do
    input <- readFile "input.txt"
    let str = concat $ lines input -- remove newlines
    let firstSegment = getFirstSegment str -- get first segment of text before first do() or don't() as this should be included in the total
    let startsWithDoStrs = (filter (checkIfStartsWith "do()") (tails str)) -- All substrings starting with do()
    let fullDoStrs = map getDoStr (firstSegment : (catMaybes $ map (stripPrefix "do()") startsWithDoStrs)) -- All substrings starting with do() and ending with do() or don't(), plus the first segment
    let startsWithMulStrs = filter (checkIfStartsWith "mul(") (concat $ map tails fullDoStrs) -- all substrings starting with "mul("
    let potentialMulStrs = map (keepUntil (==')')) startsWithMulStrs -- all substrings starting with "mul(" and ending with ")"
    let potentialFirstNums = map (keepUntil (==',')) (map (fromJust . stripPrefix "mul(") potentialMulStrs) -- get all possible firstNum strings (strings starting at end of mul( and ending at ,)
    let potentialSecondNums = map tail (map (dropWhile (/=',')) potentialMulStrs) -- get all possible secondNum strings (starting at , and ending at end of string)
    let firstNumMaybes = map (\s -> readMaybe s :: Maybe Int) potentialFirstNums
    let secondNumMaybes = map (\s -> readMaybe s :: Maybe Int) potentialSecondNums
    let mulPairs = getPairs firstNumMaybes secondNumMaybes
    let productMaybes = map mulNums mulPairs
    let total = sum $ catMaybes productMaybes
    print total
