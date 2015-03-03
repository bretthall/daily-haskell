import Data.Digits

{--

Solution to @1HaskellADay http://lpaste.net/121470 by @bretthall

From projecteuler.net, problem 36

The decimal number, 585 = 10010010012 (binary), is palindromic in both bases.

Find the sum of all numbers, less than one million, which are palindromic in
base 10 and base 2.

(Please note that the palindromic number, in either base, may not include
leading zeros.)

--}

type Base = Int

--Don't need to worry about the reversed number having leading zeroes since no numbers have leading zeroes after being put
--through the digits wringer so the reverse can't be equal to the original digits if there are trailing zeroes in the original. 
isPalindromic :: Base -> Int -> Bool
isPalindromic b n = ds == reverse ds
    where
      ds = digits b n

type Limit = Int

palindromes :: Limit -> [Int]
palindromes limit = filter (\n -> isPalindromic 10 n && isPalindromic 2 n) [1..(limit - 1)]

--some extra fromIntegral's in here since digits only works with Int, not Integer, but we want to do the 
--sum using Integer since it could be really big 
doublePalindromic :: Integer -> Integer
doublePalindromic = sum . map fromIntegral . palindromes . fromIntegral

-- *Main> palindromes 1000000
-- [1,3,5,7,9,33,99,313,585,717,7447,9009,15351,32223,39993,53235,53835,73737,585585]
-- *Main> doublePalindromic 1000000
-- 872187

--Thought maybe we'd need parallelization or to come up with a less brute force solution, but even under ghci the above takes 
--less than two seconds to run so it's good enough