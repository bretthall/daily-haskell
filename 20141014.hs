{--

From the Project Euler, problem 26.

A unit fraction contains 1 in the numerator. The decimal representation 
of the unit fractions with denominators 2 to 10 are given:

1/2	= 	0.5
1/3	= 	0.(3)
1/4	= 	0.25
1/5	= 	0.2
1/6	= 	0.1(6)
1/7	= 	0.(142857)
1/8	= 	0.125
1/9	= 	0.(1)
1/10	= 	0.1
Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It 
can be seen that 1/7 has a 6-digit recurring cycle.

Find the value of d < 1000 for which 1/d contains the longest recurring 
cycle in its decimal fraction part.

--}

import Data.List (maximumBy)

--finds the number with longest cycle, return ((cycle length, cycle), number)
maxCycle:: ((Int, String), Int)
maxCycle = maximumBy (\x y -> compare ((fst.fst) x) ((fst.fst) y)) $ zip (map longestCycle [1..1000]) ([1..1000])

--A cycle has been found when the digit and remainder repeat
--thus cycles can be at most length d for any number d (after that we run out of possible remainders without repeating)
longestCycle :: Int -> (Int, String)
longestCycle denominator = findCycle 1 1 ds (tail $ ds)
    where
      ds = digits denominator
      findCycle s _ (_:rs) [] = findCycle (s + 1) 1 rs (drop (s + 1) ds)
      findCycle _ _ [] _ = (0, "")
      findCycle s n digits1@((d1, r1):rest1) ((d2, r2):rest2) | (r2 == 0) = (0, "")
                                                              | (r2 == r1) && (d2 == d1) = (n, concatMap show $ take n $ map fst digits1)
                                                              | otherwise = findCycle s (n + 1) digits1 rest2

-- generates the decimal expansion of the given number in the form [(digit, remainder)] up to n digits
digits :: Int -> [(Int, Int)]
digits n = take n $ fracDigits n

-- generates the decimal expansion of the given number in the form [(digit, remainder)]
fracDigits :: Int -> [(Int, Int)]
fracDigits x = fd x 10
    where 
      fd _ 0 = []
      fd d n | n < d = (0, n):(fd d (n*10))
             | otherwise = let (digit, rem) = n `divMod` d in ((digit, rem):(fd x (rem*10)))

-- where the answer is of the form (6, "142857") or, if you're feeling
-- your moxie: (6, "0.(142857)")