import Prelude

type Grid = [(Integer, Char)]
type Dir = Int

gridWidth :: Integer
gridWidth = 140

gridTotalLength :: Integer
gridTotalLength = gridWidth * 140

-- Grid is flattened to 1D. A "dir" is just a number which, when added to an index, will correspond
-- to a direction e.g. x+1, y+1 is equivalent to adding gridWidth + 1.
validDirs :: [Integer]
validDirs = [(-gridWidth - 1), (-gridWidth + 1), gridWidth - 1, gridWidth + 1]

isSInThisDir :: Integer -> [Integer] -> Bool
isSInThisDir potSIdx sIdxes = potSIdx `elem` sIdxes

isAInThisDir :: Integer -> Integer -> [Integer] -> [Integer] -> Bool
isAInThisDir dir potAIdx aIdxes sIdxes = (potAIdx `elem` aIdxes) && isSInThisDir (potAIdx + dir) sIdxes

validDir :: Integer -> Integer -> Bool
validDir dir idx
    | dir == 1 = (x + 1 < gridWidth) -- x + 1
    | dir == -1 = (x - 1 >= 0) -- x - 1
    | dir == (-gridWidth - 1) = (x - 1 >= 0) && (y - 1 >= 0) -- x - 1, y - 1
    | dir == -gridWidth = (y - 1 >= 0) -- y - 1
    | dir == (-gridWidth + 1) = (x + 1 < gridWidth) && (y - 1 >= 0) -- x + 1, y - 1
    | dir == gridWidth - 1 = (x - 1 >= 0) && (y + 1 < gridWidth) -- x - 1, y + 1
    | dir == gridWidth = y + 1 < gridWidth -- y + 1
    | dir == gridWidth + 1 = (x + 1 < gridWidth) && (y - 1 < gridWidth) -- x + 1, y + 1
    where
        x = idx `rem` gridWidth
        y = idx `div` gridWidth

getIndices :: Char -> Grid -> [Integer]
getIndices search grid = map fst (filter (\(i, c) -> c == search) grid)

findDiagonal1 :: Integer -> [Integer] -> [Integer] -> Bool
findDiagonal1 aIdx mIdxes sIdxes = ((aIdx - gridWidth + 1 `elem` mIdxes) && (aIdx + gridWidth - 1 `elem` sIdxes)) || ((aIdx - gridWidth + 1 `elem` sIdxes) && (aIdx + gridWidth - 1 `elem` mIdxes))

findDiagonal2 :: Integer -> [Integer] -> [Integer] -> Bool
findDiagonal2 aIdx mIdxes sIdxes = ((aIdx - gridWidth - 1 `elem` mIdxes) && (aIdx + gridWidth + 1 `elem` sIdxes)) || ((aIdx - gridWidth - 1 `elem` sIdxes) && (aIdx + gridWidth + 1 `elem` mIdxes))

isValidA :: Integer -> [Integer] -> [Integer] -> Bool
isValidA aIdx mIdxes sIdxes
    | all (==True) (map (\d -> validDir d aIdx) validDirs) = findDiagonal1 aIdx mIdxes sIdxes && findDiagonal2 aIdx mIdxes sIdxes
    | otherwise = False

crossCount :: Grid -> Int
crossCount g = length validAs
    where
        allMIdxes = getIndices 'M' g
        allAIdxes = getIndices 'A' g
        allSIdxes = getIndices 'S' g
        validAs = filter (==True) (map (\aIdx -> isValidA aIdx allMIdxes allSIdxes) allAIdxes)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let grid = (concat . lines) input -- remove newlines, grid as one line
    let gridWithIndices = zip [0..gridTotalLength] grid
    let crossesCount = crossCount gridWithIndices
    print crossesCount
