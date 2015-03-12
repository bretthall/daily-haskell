{--

Kind permission granted by Ben Vitale to publish this puzzle here.

http://benvitalenum3ers.wordpress.com/2014/10/12/grid-4x3-numbers-from-1-to-15/

Given the following 4x3 grid:

     -------------------------
     |     |     |     |     |
     |  A  |  B  |  C  |  D  |
     |     |     |     |     |
     -------------------------
     |     |     |     |     |
     |  E  |  F  |  G  |  H  |
     |     |     |     |     |
     -------------------------
     |     |     |     |     |
     |  I  |  J  |  K  |  L  |
     |     |     |     |     |
     -------------------------

place twelve of the numbers 1 .. 15 in this grid such that

All rows sum to each other:

    A + B + C + D == E + F + G + H == I + J + K + L

All columns sum to each other:

    A + E + I == B + F + J == C + G + K == D + H + L

All diagonals of length 3 sum to each other:

    A + F + K == B + G + L == C + F + I == D + G + J

AND each of the columns (of length 3), besides equaling each other, ALSO are
equal to the diagonals of length 3 listed above.

As Neo says: "Whoa."

Data.Matrix ... 'may' be helpful here.

 --}

module Main where

import Data.Array.Unboxed (UArray, (!), listArray)
import Data.List (intercalate, permutations)
import Combinations

data Grid = Grid {numRows::Int, numCols::Int, els::UArray Int Int} deriving (Show)

getElem :: Grid -> Int -> Int -> Int
getElem grid row col = (els grid) ! (row*(numCols grid) + col)

getRow :: Grid -> Int -> [Int]
getRow grid row | row < (numRows grid) = map (getElem grid row) [0..(numCols grid - 1)]
                | otherwise = []

getCol :: Grid -> Int -> [Int]
getCol grid col | col < (numCols grid) = map (flip (getElem grid) col) [0..(numRows grid - 1)]
                | otherwise = []

getFWDiag :: Grid -> Int -> [Int]
getFWDiag grid startCol 
    | startCol <= (numSteps - 1) =  map (uncurry $ getElem grid) [(r, startCol+r) | r <- [0..numSteps]]
    | otherwise  = []
    where 
      numSteps = numCols grid - numRows grid + 1

getBWDiag :: Grid -> Int -> [Int]
getBWDiag grid startCol 
    | startCol >= numSteps =  map (uncurry $ getElem grid) [(r, startCol-r) | r <- [0..numSteps]]
    | otherwise  = []
    where 
      numSteps = numCols grid - numRows grid + 1

printGrid :: Grid -> String
printGrid grid = (intercalate "\n" $ map (printRow.(getRow grid)) [0..(numRows grid - 1)]) ++ "\n"
    where
      printRow = (intercalate " ").(map formatNum)
      formatNum n | (length $ show n) == 1 = " " ++ (show n)
                  | otherwise = show n

grids :: [Grid]
grids = map makeGrid $ concatMap permutations $ combinations 12 ([1..15]::[Int])
    where
      makeGrid = (Grid 3 4).(listArray (0,11))

rowSumRule :: Grid -> Bool
rowSumRule grid = ((rowSum 1) == rs0) && ((rowSum 2) == rs0)
    where
      rs0 = rowSum 0
      rowSum = sum.(getRow grid)

colDiagSumRule :: Grid -> Bool
colDiagSumRule grid = and [colSum 1 == cs0, colSum 2 == cs0, colSum 3 == cs0, 
                           fwDiagSum 0 == cs0, fwDiagSum 1 == cs0, 
                           bwDiagSum 2 == cs0, bwDiagSum 3 == cs0]
    where
      cs0 = colSum 0
      colSum = sum.(getCol grid)
      fwDiagSum = sum.(getFWDiag grid)
      bwDiagSum = sum.(getBWDiag grid)

rules :: Grid -> Bool
rules grid = (rowSumRule grid) && (colDiagSumRule grid)

--This will take a really long time as we have 455*12! ~ 20 BILLION different grids to test
--(455 combinations of 12 things chosen from 15 things then we need to test all the permutations of 
--each combination so multiply by 12!)
--There's probably a better way to do this, a little linear algebra should reduce the solution space
--quite a bit. Note that there will be multiple solutions since we have 14 unknowns (12 grid elements plus 
--the row and colunm/diagonal sums) with 11 linearly-independent constraints (3 row sums + 4 column sums + 
--4 diagonal sums). So really there's no need for a "whoa" above, there's lots of solutions, the hard part is 
--finding them. But brute forcing it is good practice for learning Haskell.
findGrid :: [Grid]
findGrid = filter rules grids

main = mapM_ (putStr.printGrid) findGrid

{-- 

The results should be either a populated grid, or, if you prefer, the pairs
of grid-labeled positions with their numbers

--}

numb3rs :: [Int]
numb3rs = [1..15]

data Label = A | B | C | D | E | F | G | H | I | J | K | L
   deriving (Eq, Ord, Enum, Show, Read)

magicGrid :: [Int] -> Grid -> Grid
magicGrid numbersToPlace emptyGrid = undefined

{--

So the grid represenation COULD be 

type Square = (Label, Int)
type Grid = [Square]

Or it could be something else entirely. Up to you.

 --}