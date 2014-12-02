{--

Solution to @1HaskellADay http://lpaste.net/114208

Digit cancelling fractions
Project Euler, Problem 33

The fraction 49/98 is a curious fraction, as an inexperienced mathematician in 
attempting to simplify it may incorrectly believe that 49/98 = 4/8, which is 
correct, is obtained by cancelling the 9s.

We shall consider fractions like, 30/50 = 3/5, to be trivial examples.

There are exactly four non-trivial examples of this type of fraction, less than 
one in value, and containing two digits in the numerator and denominator.

If the product of these four fractions is given in its lowest common terms, 
find the value of the denominator.

--}

import Data.Ratio
import Data.List
import Data.Maybe

data Frac = Frac {num::Int, denom::Int} deriving (Eq)

curiousFractions :: [Frac]
curiousFractions = [n // d | n <- [11..99], 
                             d <- [(n + 1)..99], -- fraction < 1 => d > n
                             n `mod` 10 /= 0, -- eliminate trivial
                             d `mod` 10 /= 0, -- eliminate trivial
                             isCurious n d]

-- *Main> curiousFractions
-- [16/64,19/95,26/65,49/98]

instance Show Frac where
    show (Frac n d) = show n ++ "/" ++ show d

(//) :: Int -> Int -> Frac
n // d = Frac n d

ratioToFrac :: Ratio Int -> Frac
ratioToFrac r = numerator r // denominator r

reduce :: Frac -> Frac
reduce (Frac n d) = ratioToFrac (n % d)

removeDigit :: Char -> Int -> Int
removeDigit c = read.delete c.show

isCurious :: Int -> Int -> Bool
isCurious n d = fromMaybe False $ do
                  md <- find (`elem` show d) $ show n
                  let wd = removeDigit md n // removeDigit md d
                  return (reduce wd == reduce (n//d))
                  

