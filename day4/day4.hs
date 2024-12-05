import Prelude

type Grid = [(Integer, Char)]

gridWidth :: Integer
gridWidth = 140

gridTotalLength :: Integer
gridTotalLength = gridWidth * 140

-- Grid is flattened to 1D. A "dir" is just a number which, when added to an index, will correspond
-- to a direction e.g. x+1, y+1 is equivalent to adding gridWidth + 1.
validDirs :: [Integer]
validDirs = [(-gridWidth - 1), -gridWidth, (-gridWidth + 1), -1, 1, gridWidth - 1, gridWidth, gridWidth + 1]

isSInThisDir :: Integer -> [Integer] -> Bool
isSInThisDir potSIdx sIdxes = potSIdx `elem` sIdxes

isAInThisDir :: Integer -> Integer -> [Integer] -> [Integer] -> Bool
isAInThisDir dir potAIdx aIdxes sIdxes = (potAIdx `elem` aIdxes) && isSInThisDir (potAIdx + dir) sIdxes

validDir :: Integer -> Integer -> Bool
validDir dir idx
    | dir == 1 = (x + 3 < gridWidth) -- x + 1
    | dir == -1 = (x - 3 >= 0) -- x - 1
    | dir == (-gridWidth - 1) = (x - 3 >= 0) && (y - 3 >= 0) -- x - 1, y - 1
    | dir == -gridWidth = (y - 3 >= 0) -- y - 1
    | dir == (-gridWidth + 1) = (x + 3 < gridWidth) && (y - 3 >= 0) -- x + 1, y - 1
    | dir == gridWidth - 1 = (x - 3 >= 0) && (y + 3 < gridWidth) -- x - 1, y + 1
    | dir == gridWidth = y + 3 < gridWidth -- y + 1
    | dir == gridWidth + 1 = (x + 3 < gridWidth) && (y - 3 < gridWidth) -- x + 1, y + 1
    where
        x = idx `rem` gridWidth
        y = idx `div` gridWidth

isXmasInThisDir :: Integer -> Integer -> [Integer] -> [Integer] -> [Integer] -> Bool
isXmasInThisDir dir xIdx mIdxes aIdxes sIdxes
    | validDir dir xIdx = (potMIdx `elem` mIdxes) && isAInThisDir dir (potMIdx + dir) aIdxes sIdxes
    | otherwise = False
        where
            potMIdx = xIdx + dir

getIndices :: Char -> Grid -> [Integer]
getIndices search grid = map fst (filter (\(i, c) -> c == search) grid)

getXmasCount :: Grid -> Int
getXmasCount g = length hasXmas
    where
        allXIdxes = getIndices 'X' g
        allMIdxes = getIndices 'M' g
        allAIdxes = getIndices 'A' g
        allSIdxes = getIndices 'S' g
        allXIdxesWithDirs = [(xIdx, dir) | xIdx <- allXIdxes, dir <- validDirs]
        hasXmas = filter (==True) (map (\(xIdx, dir) -> isXmasInThisDir dir xIdx allMIdxes allAIdxes allSIdxes) allXIdxesWithDirs)


main :: IO ()
main = do
    input <- readFile "input.txt"
    let grid = (concat . lines) input -- remove newlines, grid as one line
    let gridWithIndices = zip [0..gridTotalLength] grid
    let xmasCount = getXmasCount gridWithIndices
    print xmasCount
