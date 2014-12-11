{--

Solution to @!HaskellADay http://lpaste.net/116216

http://rosalind.info/problems/fib/

Rabbits and Recurrence Relations solved by 5713
Topics: Combinatorics, Dynamic Programming

Wascally Wabbits

Problem

A sequence is an ordered collection of objects (usually numbers), which are
allowed to repeat. Sequences can be finite or infinite. Two examples are the
finite sequence (π,−2√,0,π) and the infinite sequence of odd numbers
(1,3,5,7,9,…). We use the notation an to represent the n-th term of a sequence.

A recurrence relation is a way of defining the terms of a sequence with
respect to the values of previous terms. In the case of Fibonacci's rabbits
from the introduction, any given month will contain the rabbits that were
alive the previous month, plus any new offspring. A key observation is that
the number of offspring in any month is equal to the number of rabbits that
were alive two months prior. As a result, if Fn represents the number of


rabbit pairs alive after the n-th month, then we obtain the Fibonacci sequence
having terms Fn that are defined by the recurrence relation Fn=Fn−1+Fn−2 (with
F1=F2=1 to initiate the sequence). Although the sequence bears Fibonacci's
name, it was known to Indian mathematicians over two millennia ago.

When finding the n-th term of a sequence defined by a recurrence relation, we
can simply use the recurrence relation to generate terms for progressively
larger values of n. This problem introduces us to the computational technique
of dynamic programming, which successively builds up solutions by using the
answers to smaller cases.

Given: Positive integers n≤40 and k≤5.

Return: The total number of rabbit pairs that will be present after n months
if we begin with 1 pair and in each generation, every pair of reproduction-age
rabbits produces a litter of k rabbit pairs (instead of only 1 pair).

Sample Dataset
5 3

Sample Output
19 --}

-- The recurrence is F_n = F_{n - 1} + kF_{n - 2} 
module Main where

import Data.List
import System.Environment
import Timing -- http://lpaste.net/116265

--naive version that recalcs things when it doesn't need to
naiveRecur :: Integer -> Integer -> Integer
naiveRecur n k | n == 0 = 0
               | n == 1 = 1
               | n == 2 = 1
               | otherwise = naiveRecur (n - 1) k + k * naiveRecur (n - 2) k

-- *Main Data.List> naiveRecur 5 3
-- 19
-- => It works, but...
-- *Main Data.List> naiveRecur 40 5
--   C-c C-cInterrupted.
-- Had to interrupt since it was still runnign after 10 seconds
-- That's to be expected since it is recalculating all the values that come before on each iteration

--Instead lets unfold saving the previous two values for use in calculating the current value
allVals :: Integer -> [(Integer, Integer)]
allVals k = unfoldr next (2, 1, 0)
    where 
      next (i, fn1, fn2) = Just ((i, fn), (i + 1, fn, fn1))
          where
            fn = fn1 + k*fn2

unfoldRecur :: Integer -> Integer -> Integer
unfoldRecur n k | n == 0 = 0
                | n == 1 = 1
                | otherwise = snd $ head $ drop (fromIntegral n - 2) $ allVals k

-- *Main Data.List> unfoldRecur 5 3
-- 19
-- => This works too

-- *Main Data.List> unfoldRecur 40 5
-- 148277527396903091
-- => Got the answer instantly

-- We could also eliminate all the list business and do the recursion manually
manualRecur :: Integer -> Integer -> Integer
manualRecur n k = recur 2 1 0
    where 
      recur i fn1 fn2 | i > n = fn1
                      | otherwise = recur (i + 1) (fn1 + k*fn2) fn1

-- *Main Data.List> manualRecur 5 3
-- 19
-- => This works too

main = do
  [nS, kS] <- getArgs
  let n = read nS
      k = read kS
  unfolded <- timeIt (unfoldRecur n k)
  manual <- timeIt (manualRecur n k)
  putStrLn $ "unfolded: " ++ show (snd unfolded) ++ "ns"
  putStrLn $ "manual:   " ++ show (snd manual) ++ "ns"

-- bhall $./20141211.exe 4000 1
-- unfolded: 1414181ns
-- manual:   760669ns

-- bhall $./20141211.exe 40000 1
-- unfolded: 81040885ns
-- manual:   31811984ns

-- bhall $./20141211.exe 400000 1
-- unfolded: 9929353723ns
-- manual:   2807757984ns

-- => manualRecur is much faster for large n