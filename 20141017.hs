{--

From Yesterday's @OnThisDayInMath tweet:

"The 289th day of the year; 289 is a Friedman number, since

(8 + 9) ^ 2 == 289

an expression using all its own digits."

Find all the Friedman numbers in a year.

[ed: a MAYAN year?]

--}

{--

A further elaboration is on "Pat's Blog":

http://pballew.blogspot.com/2014/10/on-this-day-in-math-october-16.html

"... A Friedman number is an integer which, in a given base, is the result 
of an expression using all its own digits in combination with any of the 
four basic arithmetic operators (+, −, ×, ÷) and sometimes exponentiation.

Students might try to find the first few multi-digit Friedman numbers."

Okay, "students": do-it-to-it!

A bonus problem for proving a Friedman number is available at 
http://lpaste.net/112709

--}

import Data.List (permutations)
import Combinations

nums :: [Int]
nums = [10..365]

--Generates a list of all the list of numbers that can be made from the digits of the given number.
--We only support 2 and 3 digit numbers since thise are the only ones of interest for the problem at hand.
subNums :: Int -> [[Int]]
subNums x | n == 3 = digits:(map (\(x,y)->[toInt x, toInt y]) $ map (splitAt 1) $ permutations s)
          | n == 2 = digits:[]
          | otherwise = undefined
    where 
      s = show x
      digits = map (read.(:"")) s
      n = length s
      toInt x = (read x)::Int

safeDiv :: Int -> Int -> Int
safeDiv x y | y == 0 = 0
            | otherwise = x `div` y

safePower :: Int -> Int -> Int
safePower x y | y < 0 = 0
              | otherwise = x ^ y
ops :: [Int -> Int -> Int]
ops = [(+), (-), (*), safeDiv, safePower]

-- Tests whether the given number is Friendman or not.
-- We take all the possible numbers that can be generated from the given number's digits and do every combination
-- of perumtations of those numbers with every operator from the ops list (for sets of three numbers we use pairs of the 
-- operators). When we have three numbers we also do both possible associations of the operators.
isFriedman :: Int -> Bool
isFriedman num = not $ null $ filter id $ map subFN $ subNums num
    where 
      subFN xs@(x1:x2:[]) = not $ null $ filter (== num) $ concatMap (\op -> map (eval2 op) $ permutations xs) ops
      subFN xs@(x1:x2:x3:[]) = not $ null $ filter (== num) $ concatMap (\op-> concatMap (eval3 op) $ permutations xs) $ concatMap permutations $ combinations 2 ops
      eval2 op (x1:x2:[]) = (x1 `op` x2)
      eval3 (op1:op2:[]) (x1:x2:x3:[])  = ((x1 `op1` x2) `op2` x3):(x1 `op1` (x2 `op2` x3)):[] 

--Generates the list of Friedman numbers.
friedmanNums :: [Int]
friedmanNums = map fst $ filter snd $ map (\x -> (x, isFriedman x)) nums