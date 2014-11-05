{--
@1HaskellADay http://lpaste.net/113798

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
import Data.Ratio (Ratio)

diagonalValues :: [Int]
diagonalValues = unfoldr nextValue (1, 1)
    where
      nextValue (c, n) = Just $ (c, (c + 2*n, n + 1))

soln :: Int -> Int
soln spiralSize = sum $ take spiralSize diagonalValues

-- *Main> soln 1001
-- 334335001

--OR... we could derive a closed form for the recurrence of the diagonal values
-- it's not hard to see from the above that T_n (the n'th diagonal value) is
-- T_n = T_(n - 1) + 2*n
-- where first value is
-- T_0 = 1
-- Writing out the first few T_n values makes it obvious that 
-- T_n = 1 + Sum(k = 1 -> n) 2k = 1 + 2(n(n+1)/2) = n^2 + n + 1
-- Then the diagonal sum can be derived from the usual fomulas for summing 1, n and n^2

--Don't really need it, but just in case lets generate the diagonal values in closed form and check our math
diagonalValueClosed :: Int -> Int
diagonalValueClosed n = n*n + n + 1

quickDiagonalValues :: [Int]
quickDiagonalValues = map diagonalValueClosed [0..]

-- *Main> filter (uncurry (/=)) $ take 100000 $ zip quickDiagonalValues diagonalValues
-- []
-- enough proof for me

-- Sum (k = 0 -> n) 1 = n + 1
sum1 n = n + 1
-- Sum (k = 0 -> n) k = n(n+1)/2
sumN n = n*(n+1) `div` 2
-- Sum (k = 0 -> n) k^2 = n(n+1)(2n+1)/6
sumN2 n = n*(n+1)*(2*n + 1) `div` 6

quickSoln :: Int -> Int
quickSoln n = (sumN2 m) + (sumN m) + (sum1 m)
    where 
      m = n - 1

-- *Main> filter (uncurry (/=)) $ take 1000 $ zip (map soln [1..]) (map quickSoln [1..])
-- []
-- enough proof for me

-- *Main> quickSoln 1001
-- 334335001




