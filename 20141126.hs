{--

Solution to @1HaskellADay http://lpaste.net/115096

Got a question for ya!

On This Day in Math @OnThisDayinMath
The 327th day of the year; 327 is the largest number n so that 
n, 2n, and 3n together contain every digit from 1-9 exactly once.

So, okay, we know that about 327. What is the smallest three-digit
number that meets those conditions? What are all the numbers that
meet those conditions?

--}

module Main where

import Data.List (sort)

digits :: Int -> [Int]
digits x = [hs, ts, os]
    where
      hs = x `div` 100
      xMHs = x - hs*100
      ts = xMHs `div` 10
      os = xMHs - ts*10


onThisDayInMath :: [Int]
onThisDayInMath = [x | x <- [100..327], isGood x]
    where 
      isGood x = all (`elem` allDigits x) [1..9] 
      allDigits x = digits x ++ digits (2*x) ++ digits (3*x)

-- *Main> onThisDayInMath
-- [192,219,273,327]

main = print onThisDayInMath