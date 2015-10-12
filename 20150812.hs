import Data.Function (fix)

{--

So, yesterday we said divby10 is the composition of divby2 and divby5 and that, then,
divby30 is the composition of that (divby10) and divby3.

Something interesting about the rule for divby3.

Divby3 is this: 

for n, the sum of n's digits is divisible by 3 -| n is divisible by 3

Hm, but what if the sum of n's digits is some big number? Does that suggest that
the sum of the digits of n's sum of digits are divisible by 3 mean that n is divisible
by 3?

In short, is divby3 a fixpoint?

Show that divby3 is a fixpoint. Or, show that divby3 is NOT a fixpoint.

That is, for any n, divby3 for that n is true if the sum reduces, eventually, to
0 (good luck), 3, 6, or 9. For this statement to hold, you must show (for yourself)
that sum is reductive. The proof is very, very easy, but something you need that
for a number of x digits where x > 1, that sum digits-of-n (x digits) reduces to m 
which is 1) less than n and 2) has x or fewer digits. Can you show this as a bonus,
perhaps?

You may use MonadFix, the function fix, or however so your heart desires to define 
this fixpoint of divby3.
--}

digits :: Int -> [Int]
digits = map (read.(:"")) . show

divby3 :: Int -> Bool
divby3 x | x < 0 = divby3 (-x) -- sign doesn't matter here and digits only works for positive numbers
         | s `elem` [3,6,9]  = True
         | otherwise = False
         where
           s = sums x
           sums n | length ds == 1 = head ds
                  | otherwise = sums $ sum ds
                  where
                    ds = digits n

sumDigits :: Int -> Int
sumDigits n | n < 10 = n
            | otherwise = sum $ digits n

sums :: Int -> [Int]
sums n | n < 10 = [n]
       | otherwise = let s = sumDigits n in s : sums s

-- fixdivby3 :: Int -> Bool
-- fixdivby3 n = (fix sumDigits $ n) `elem` [0, 3, 6, 9]

sumRec :: (Int -> Int) -> (Int -> Int)
sumRec r n = if n < 10 then n else r $ sum $ digits n

fixdivby3 = flip elem [3,6,9,0] . fix sumRec

-- fix sumRec n = (sumRec $ sumRec $ sumRec $ ...) n

-- fix :: (a -> a) -> a
-- (Int -> Int) -> Int -> Int
-- ((Int -> Int) -> (Int -> Int)) -> (Int -> Int)

-- show that fixdivby3 works (or does not work) for nums, below...

nums :: [Int]               -- generated from random.org
nums = [11343,8066,92660,4831,80343,67748,65860,4322,19739,17994]

-- by showing filter ((0 ==) . flip mod 3) nums == filter fixdivby3 nums

-- Tomorrow, we show that ALL divisible rules are actually one rule, viewed through
-- a certain ... *ahem* ... 'lens.'