module Main where

import Data.List
import System.Environment (getArgs)

{--

Inspired from a tweet on October 1st, 2014:

James Tanton @jamestanton  28m28 minutes ago
3!=1+2+3
4!=1=2+3+4+6+8
What's the longest sum you can write for 5! as a sum of distinct factors 
of itself? 6!? 7!?

 --}

fact n = product [1..n]

factors n = filter (\x -> n `mod` x == 0) [1..(n - 1)]

sumEquals n xs = sum xs == n

-- Works but for base > 5 it won't finish in a reasonable amount of time ("reasonable" being defined very loosely).
-- The problem is that subsequences generates the smallest sequences first so we have to go through all of 
-- them to get to the longest sequence. Instead we shoudl be starting with the longest and quitting when we find the first 
-- valid sequence. Plus, if we find a sequence that sums to something less than what we're looking for we can ignore any 
-- sequences that are generated from that sequence by removing elements.
distinctFactorsSum :: Int -> [Int]
distinctFactorsSum base = head $  filter (sumEquals f) $ sortBy (\x y -> compare (length y) (length x)) $ subsequences $ factors f
    where 
      f = fact base

-- where input is x from x! and output is out = [1,2,3, ...], or whatever the
-- sequence where: 

summer :: Int -> [Int] -> Bool
summer base sequence = product [1 .. base] == sum sequence

-- summer x ans ~> True

-- for this problem the (uninforced) range of base is in [3 .. 7]

main = do
  args <- getArgs
  putStrLn $ (args !! 0) ++ "! = " ++ (intercalate " + " $ map show (distinctFactorsSum ((read (args !! 0))::Int)))
