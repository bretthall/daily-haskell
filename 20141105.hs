{--

Number spiral diagonals, from Project Euler, Problem 28

Starting with the number 1 and moving to the right in a clockwise direction a 
5 by 5 spiral is formed as follows:

21_22 23 24 25_
20  7_ 8  9_10
19  6  1_ 2 11
18  5_ 4  3_12
17_16 15 14 13_

It can be verified that the sum of the numbers on the diagonals is 101.

What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral 
formed in the same way?

--}

import Data.List (unfoldr)

diagonalValues :: [Int]
diagonalValues = unfoldr nextValue (1, 1)
    where
      nextValue (c, n) = Just $ (c, (c + 2*n, n + 1))

soln :: Int -> Int
soln spiralSize = sum $ take spiralSize diagonalValues

-- *Main> soln 1001
-- 334335001