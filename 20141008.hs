module Main where

{--

From Project Euler, problem 18:

By starting at the top of the triangle below and moving to adjacent numbers 
on the row below, the maximum total from top to bottom is 23.

        3
       7 4
      2 4 6
     8 5 9 3

That is, 3 + 7 + 4 + 9 = 23.

Find the maximum total from top to bottom of the triangle below:

                75
               95 64
              17 47 82
            18 35 87 10
           20 04 82 47 65
          19 01 23 75 03 34
         88 02 77 73 07 63 67
        99 65 04 28 06 16 70 92
       41 41 26 56 83 40 80 70 33
      41 48 72 33 47 32 37 16 94 29
     53 71 44 65 25 43 91 52 97 51 14
    70 11 33 28 77 73 17 78 39 68 17 57
   91 71 52 38 17 14 91 43 58 50 27 29 48
  63 66 04 68 89 53 67 30 73 16 69 87 40 31
 04 62 98 27 23 09 70 98 73 93 38 53 60 04 23

[ed: My triangle's a little lopsided ... eh.]

NOTE: As there are only 16384 routes, it is possible to solve this problem 
by trying every route. However, Problem 67, is the same challenge with a 
triangle containing one-hundred rows; it cannot be solved by brute force, and 
requires a clever method! ;o)

Okay, you clever path-puzzle-solvers! Have at it!

--}
import Data.Array.Unboxed (UArray, (!), listArray)
import Data.List (maximumBy, intercalate)

type TriangleRow = UArray Int Int
type TriangleOfNumbers = [TriangleRow]

rowFromList :: [Int] -> TriangleRow
rowFromList xs = listArray (0, ((length xs) - 1)) xs

triangle = [rowFromList [75],
            rowFromList [95, 64],
            rowFromList [17, 47, 82],
            rowFromList [18, 35, 87, 10],
            rowFromList [20, 04, 82, 47, 65],
            rowFromList [19, 01, 23, 75, 03, 34],
            rowFromList [88, 02, 77, 73, 07, 63, 67],
            rowFromList [99, 65, 04, 28, 06, 16, 70, 92],
            rowFromList [41, 41, 26, 56, 83, 40, 80, 70, 33],
            rowFromList [41, 48, 72, 33, 47, 32, 37, 16, 94, 29],
            rowFromList [53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14],
            rowFromList [70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57],
            rowFromList [91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48],
            rowFromList [63, 66, 04, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31],
            rowFromList [04, 62, 98, 27, 23, 09, 70, 98, 73, 93, 38, 53, 60, 04, 23]]

type Path = [Int]

paths :: TriangleOfNumbers -> [Path]
paths (r:rs) = map ((r!0):) $ buildPaths 0 rs
    where
      buildPaths i (r:[]) = [[r!i], [r!(i + 1)]]
      buildPaths i (r:rs) = (map ((r!i):) $ buildPaths i rs) ++ (map ((r!(i + 1)):) $ buildPaths (i + 1) rs)
        
compareSums xs ys = compare (sum xs) (sum ys)

maxTotalPath :: TriangleOfNumbers -> [Int]
maxTotalPath triangle = maximumBy compareSums $ paths triangle

main = do
  let maxPath = maxTotalPath triangle
  putStrLn $ (intercalate " + " $ map show maxPath) ++ " = " ++ (show $ sum maxPath)
