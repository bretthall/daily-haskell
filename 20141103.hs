import Combinations
import Data.List (permutations)

allUnique :: (Eq a) => [a] -> Bool
allUnique [_] = True
allUnique (x:xs) | x `elem` xs = False
                 | otherwise = allUnique xs

colsGood :: (Eq a) => [[a]] -> Bool
colsGood rows | length (head rows) > 0 = (allUnique $ map head rows) && (colsGood $ map tail rows)
              | otherwise = True

type Row = String
possibleRows :: [Row]
possibleRows = permutations "SNOW"

type Grid = [Row]

addRows :: Int -> Grid -> [Row] -> [Grid]
addRows n g (r:rs) | (length g) == n = [g]
                   | colsGood (r:g) = (addRows n (r:g) rs) ++ (addRows n g rs)
                   | otherwise = addRows n g rs
addRows _ _ [] = []

getDiagonal :: Grid -> String
getDiagonal g = getDiagonal' 0 g
    where 
      getDiagonal' n (r:rs) = (r !! n):(getDiagonal' (n + 1) rs)
      getDiagonal' _ [] = []

checkDiagonal :: Grid -> Bool
checkDiagonal = allUnique.getDiagonal

possibleGrids :: [Grid]
possibleGrids = filter checkDiagonal $ addRows 4 [] possibleRows

-- *Main> possibleGrids
-- [["OWSN","NSWO","WONS","SNOW"],["SOWN","WNSO","NWOS","OSNW"]]
