{--

A solution to @1HaskellADay http://lpaste.net/114158

From the funWithNumb3rs site, a problem:

(1) A is a 3-digit perfect square
    B is a 3-digit perfect cube
    C is a 4-digit triangular number

A, B and C combined use 10 different digits (no leading zero)

WHAT ARE A, B, and C?

(2) X is a 3-digit perfect square (again)
    Y is a 3-digit TRIANGULAR number
    Z is a 4-digit PERFECT CUBE

(ya see the difference?)

X, Y and Z combined use 10 different digits (no leading zero)

WHAT ARE X, Y, AND Z?

... AND HOW DO _YOU_ PRONOUNCE 'Z'? ;)

--}

import Data.List(sort)

abc :: [[Int]]
abc = [[a,b,c] | a <- possibleAs, b <- possibleBs, c <- possibleCs, checkDigits [a,b,c]]

-- *Main> abc
-- [[784,125,6903],[784,512,6903]]

xyz :: [[Int]]
xyz = [[x,y,z] | x <- possibleXs, y <- possibleYs, z <- possibleZs, checkDigits [x,y,z]]

-- *Main> xyz
-- [[256,780,4913],[576,820,4913],[625,780,4913]]

{--

So, ya know what a square and a cube are, right?

A triangular number is this: https://en.wikipedia.org/wiki/Triangular_number

or this: --}

tri :: Int -> Int
tri n = div (n * (n + 1)) 2

-- which is also this:

tri' :: Int -> Int
tri' n = sum [0.. n] 

tris :: [Int]
tris = map tri [1..]

-- source list limits chosen to generate numbers with the correct number of digits

possibleAs :: [Int]
possibleAs = map (\x -> x*x) [10..31]

possibleBs :: [Int]
possibleBs = map (\x -> x*x*x) [5..9]

possibleCs :: [Int]
possibleCs = map tri [45..140]

possibleXs :: [Int]
possibleXs = possibleAs

possibleYs :: [Int]
possibleYs = map tri [14..44]

possibleZs :: [Int]
possibleZs = map (\x -> x*x*x) [10..21]

checkDigits :: [Int] -> Bool
checkDigits = (== "0123456789").sort.(concatMap show)

-- neato! Impress your friends with triangular numbers!