import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ratio

{--
Solution to @1HaskellADay http://lpaste.net/120320

So, today is tomorrow.

From February 12th's entry in the Mensa Genuis Quiz-a-Day Book:

You've had a tough time lately: birthdays, weddings, what-all, have just about
brought your finances down to your household piggy bank. With your trusty
broad-blade knife, you manage to extract quite a few coins, for a total of $16.
To your surprise, you have exactly the same number of half-dollars, quarters,
and nickles. How many of each do you have? --}

data Coin = HalfDollar | Quarter | Nickle
   deriving (Eq, Ord, Show)

value :: Coin -> Rational
value Quarter = 25 % 100
value HalfDollar = 50 % 100
value Nickle = 5 % 100

type Count = Integer

-- we could do the algebra but the computer can probably burn through the possibilities and find us the answer quicker than that
possible :: [[Integer]]
possible = [[numerator n, numerator n, numerator n] | 
            n <- [0..32], -- $16 = 32 half dollars
            n * (value HalfDollar) + n * (value Quarter) + n * (value Nickle) == 16]

sixteenDollarsInChange :: Map Coin Count
sixteenDollarsInChange = Map.fromList $ zip [HalfDollar, Quarter, Nickle] $ head possible

{--
*Main> sixteenDollarsInChange
fromList [(HalfDollar,20),(Quarter,20),(Nickle,20)]
--}