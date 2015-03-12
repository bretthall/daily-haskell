{--

From Ben Vitale's tweet on Friday, Oct 17th

A, B, and C are 3 digit numbers.

A < B < C

A equals seven times a prime
B equals eight pimes a prime
C equals nine times a prime

A, B, and C combined contain each of the digits, 1-9, exactly once

Multiplying the three numbers (A, B, C) will give you an answer that contains
the digits 1 to 9 eactly onces

Determine A, B, and C! --}

import Data.List (permutations, sort)
import Data.Numbers.Primes (isPrime)

digits = "123456789"

type NumSet = (Int, Int, Int)

allNums :: [NumSet]
allNums = map toNums $ permutations digits
    where
      toNums (d1:d2:d3:d4:d5:d6:d7:d8:d9:[]) = (read (d1:d2:d3:[]), read (d4:d5:d6:[]), read (d7:d8:d9 :[]))

orderingRule :: NumSet -> Bool
orderingRule (a, b, c) = (a < b) && (b < c)

inOrder :: [NumSet] -> [NumSet]
inOrder = filter orderingRule 

productHasDigits :: NumSet -> Bool
productHasDigits (a, b, c) = sort (show (a*b*c)) == digits

hasRightProduct :: [NumSet] -> [NumSet]
hasRightProduct = filter productHasDigits

aIs7TimesPrime :: NumSet -> Bool
aIs7TimesPrime (a, _, _) = isPrime $ a `div` 7

bIs8TimesPrime :: NumSet -> Bool
bIs8TimesPrime (_, b, _) = isPrime $ b `div` 8

cIs9TimesPrime :: NumSet -> Bool
cIs9TimesPrime (_, _, c) = isPrime $ c `div` 9

rightTimesPrimes :: [NumSet] -> [NumSet]
rightTimesPrimes = (filter cIs9TimesPrime).(filter bIs8TimesPrime).(filter aIs7TimesPrime)

-- *Main> rightTimesPrimes $ hasRightProduct $ inOrder allNums
-- [(413,568,927)]