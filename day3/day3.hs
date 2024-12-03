import Prelude
import Data.String
import Data.List
import Data.Maybe
import Text.Read

validChars :: [Char]
validChars = ['m', 'u', 'l', '(', ')'] ++ (map (head . show) [1..9])

data MulPair = MulPair (Maybe Int) (Maybe Int)

keepUntil :: (a -> Bool) -> [a] -> [a]
keepUntil p (x:xs)
    | (p x) = []
    | otherwise = x : keepUntil p xs



checkIfStartsWith :: String -> String -> Bool
checkIfStartsWith search = isPrefixOf search

-- parseNum :: String -> String
-- parseNum [] = []
-- parseNum (c:cs)
--     | isDigit c = c : parseNum cs
--     | otherwise = []

-- getFirstNum :: String -> Maybe Int
-- checkHasFirstNum s
--     | (n@(parseNum (stripPrefix "mul(")) /= []) = Just n
--     | otherwise = Nothing

-- getSecondNum :: String -> Maybe Int
-- checkHasSecondNum s
--     | (n@(parseNum $ tail (dropWhile (/=',') s)) /= []) = Just n
--     | otherwise = Nothing

getPairs :: [Maybe Int] -> [Maybe Int] -> [(Maybe Int, Maybe Int)]
getPairs [] _ = []
getPairs _ [] = []
getPairs (a:as) (b:bs) = (a, b) : getPairs as bs

mulNums :: (Maybe Int, Maybe Int) -> Maybe Int
mulNums ((Just a),(Just b)) = Just (a * b)
mulNums (_,_)  = Nothing

main :: IO ()
main = do
    input <- readFile "input.txt"
    let str = concat $ lines input -- remove newlines
    let startsWithMulStrs = filter (checkIfStartsWith "mul(") (tails str) -- all substrings starting with "mul("
    let potentialMulStrs = map (keepUntil (==')')) startsWithMulStrs -- all substrings starting with "mul(" and ending with ")"
    let potentialFirstNums = map (keepUntil (==',')) (map (fromJust . stripPrefix "mul(") potentialMulStrs) -- get all possible firstNum strings 
    let potentialSecondNums = map tail (map (dropWhile (/=',')) potentialMulStrs) -- get all possible secondNum strings 
    let firstNumMaybes = map (\s -> readMaybe s :: Maybe Int) potentialFirstNums
    let secondNumMaybes = map (\s -> readMaybe s :: Maybe Int) potentialSecondNums
    let mulPairs = getPairs firstNumMaybes secondNumMaybes
    let productMaybes = map mulNums mulPairs
    let total = sum $ catMaybes productMaybes
    print total
