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

nums :: [Int]
nums = [10..365]

combinations :: Int -> [a] -> [[a]]
combinations n as = combinations' (length as) n as
    where 
      combinations' _ 1 as = map (:[]) as
      combinations' l n v@(a:as) | l > n = (map (a:) (combinations' (l-1) (n-1) as)) ++ (combinations' (l-1) n as)
                                 | otherwise = [v]

subNums :: Int -> [[Int]]
subNums x | n == 3 = digits:(map (\(x,y)->[toInt x, toInt y]) $ map (splitAt 1) $ permutations s)
          | n == 2 = digits:[]
          | otherwise = undefined
    where 
      s = show x
      digits = map read $ map (:"") s
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

isFriedman :: Int -> Bool
isFriedman num = not $ null $ filter id $ map subFN $ subNums num
    where 
      subFN xs@(x1:x2:[]) = not $ null $ filter (== num) $ concatMap (\op -> map (eval2 op) $ permutations xs) ops
      subFN xs@(x1:x2:x3:[]) = not $ null $ filter (== num) $ concatMap (\op-> concatMap (eval3 op) $ permutations xs) $ concatMap permutations $ combinations 2 ops
      eval2 op (x1:x2:[]) = (x1 `op` x2)
      eval3 (op1:op2:[]) (x1:x2:x3:[])  = ((x1 `op1` x2) `op2` x3):(x1 `op1` (x2 `op2` x3)):[] 

friedmanNums :: [Int]
friedmanNums = map fst $ filter snd $ map (\x -> (x, isFriedman x)) nums