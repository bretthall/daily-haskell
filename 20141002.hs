module Main where

import Data.List

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