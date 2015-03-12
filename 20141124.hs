{--

Solution to @!HaskellADay http://lpaste.net/114816

Circular Primes
Project Euler, problem 35

The number, 197, is called a circular prime because all rotations of the
digits: 197, 971, and 719, are themselves prime.

[ed: n.b. they say 'rotations' NOT 'permutations'!]

There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71,
73, 79, and 97.

How many circular primes are there below one million?

--}

import Data.Numbers.Primes
import Data.List (unfoldr)

rotations :: [a] -> [[a]]
rotations as = unfoldr nextRotation (l, cycle as)
    where
      l = length as
      nextRotation (0, _) = Nothing
      nextRotation (n, cs) = Just (take l cs, (n - 1, tail cs))  

isCircularPrime :: Int -> Bool
isCircularPrime = all isPrime.map read.tail.rotations.show 
-- skipped the first number above since we already know that it's prime

circularPrimesIn :: [Int] -> [Int]
circularPrimesIn = filter isCircularPrime 

-- *Main Data.Numbers.Primes> circularPrimesIn $ takeWhile (< 1000000) primes
-- [2,3,5,7,11,13,17,31,37,71,73,79,97,113,131,197,199,311,337,373,719,733,919,971,991,1193,1931,3119,3779,7793,7937,9311,9377,11939,19391,19937,37199,39119,71993,91193,93719,93911,99371,193939,199933,319993,331999,391939,393919,919393,933199,939193,939391,993319,999331]
-- *Main Data.Numbers.Primes> length it
-- 55
