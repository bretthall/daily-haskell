{--
Solution to @1HaskellADay http://lpaste.net/121528 by @bretthall

From Project Euler

Truncatable primes
Problem 37
The number 3797 has an interesting property. Being prime itself, it is 
possible to continuously remove digits from left to right, and remain prime at 
each stage: 3797, 797, 97, and 7. Similarly we can work from right to left: 
3797, 379, 37, and 3.

Find the sum of the only eleven primes that are both truncatable from left to 
right and right to left.

NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.

 --}
import Data.Numbers.Primes
--Let's use vectors for kicks
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Data.Vector.Generic as GV
import Control.Monad

type Base = Int

--need our own digits stuff for vectors

numDigits :: Integral a => a -> a -> Int
numDigits b n = num n 0
    where
      num i d | i == 0 = d
              | otherwise = num (i `div` b) (d + 1)

digitsVec :: (Integral a, V.Unbox a) => a -> a -> V.Vector a
digitsVec b n = V.create $ do
                  let numDs = numDigits b n
                  v <- MV.new numDs
                  fill (numDs - 1) n v
                  return v
    where
      fill i n v = do
        let (rest, digit) = quotRem n b
        MV.write v i digit
        when (i > 0) $ fill (i - 1) rest v

unDigitsVec :: (Integral a, V.Unbox a) => a -> V.Vector a -> a
unDigitsVec b v = V.foldl' (\a v -> a*b + v) 0 v

truncations :: Int -> [Int]
truncations n = leftTruncations ds ++ rightTruncations ds
    where
      ds = digitsVec 10 n
      leftTruncations ts | numTs > 1 = let ts' = V.unsafeTail ts in unDigitsVec 10 ts' : leftTruncations ts'
                         | otherwise = []
          where
            numTs = V.length ts
      rightTruncations ts | numTs > 1 = let ts' = V.unsafeInit ts in unDigitsVec 10 ts' : rightTruncations ts'
                          | otherwise = []
          where
            numTs = V.length ts

truncatablePrime :: Int -> Bool
truncatablePrime prime = all isPrime $ truncations prime

truncatablePrimes :: [Int]
truncatablePrimes = filter truncatablePrime $ dropWhile (< 10) primes

-- *Main> take 12 truncatablePrimes
-- [23,37,53,73,313,317,373,797,3137,3797,739397  C-c C-cInterrupted.
-- Gave it a couple of minutes to chew on the above, so I'll accept that there's only 11 
-- (as far as reasonable computing time goes). I'll assume that there's a proof somewhere that 
-- there's only 11 such numbers.

sum11TruncPrimes :: Integer
sum11TruncPrimes = sum $ map fromIntegral $ take 11 truncatablePrimes

-- *Main> sum11TruncPrimes
-- 748317

