{--
Solution to @1HaskellADay http://lpaste.net/119791

From a tweet from today:

James Tanton @jamestanton  28m28 minutes ago
110 in base 2 ends with 110.
210 in base 3 ends with 210.
Three-digit example for base 4? Base 5? Base 6?

--}

import Data.List (find)

type Base = Int
type Base10 = Int
type BaseNDigit = Int 
 
-- Calculated digits of a number in the given base, the digits come back in the order of increasing powers of the base,
-- e.g. digits b n = [a0,a1,a2...] where n = a0*b^0 + a1*b^1 + s2*b^2 + ...
-- note: this function is only defined for POSITIVE INTEGERS
digits :: Base -> Base10 -> [BaseNDigit]
digits b n = digits' b 1 n
    where
      -- d_n = floor(x/b^n) mod b => n'th digit of x in base b
      digits' b c n = (n `div` c `mod` b) : digits' b (c*b) n

-- This ONLY works for base <= 10
isGood :: (Base10, [BaseNDigit]) -> Bool
isGood (n, ds) = (take 3 $ reverse $ show n) == (concatMap show $ take 3 ds)

threeDigits :: Base -> Maybe Base10
threeDigits base = case take 1 $ allThreeDigits base of
                     n:_ -> Just n
                     [] -> Nothing
      
allThreeDigits :: Base -> [Base10]
allThreeDigits base = map fst $ filter isGood $ map (\n -> (n, digits base n)) [1..]

-- e.g.: threeDigits 2 ~> 110 threeDigits 3 ~> 210 threeDigits 4 ~> ?

{--
 Why only find one? Instead lets find a bunch
*Main> take 6 $ allThreeDigits 4
[3320,3321,3322,3323,4120,4121]
*Main> take 6 $ allThreeDigits 5
[1000,1001,1002,1003,1004,2000]
*Main> take 6 $ allThreeDigits 6
[1140,1141,1142,1143,1144,1145]
--}