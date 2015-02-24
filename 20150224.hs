import Control.Monad
import Math.NumberTheory.Powers
import Data.Digits
import Data.List (unfoldr)

{--
Solution to @1HaskellADay http://lpaste.net/121060

Okay, so after reading my posting yesterday from square-to-square-by-one,
Benjamin Vitale @BenVitale  3h3 hours ago wrote this:
@1HaskellADay Find a square number that can be transformed into a cube if ONE 
digit is increased by 1.

So, it's a square-to-cube problem now.

But that's no problem. Right? It's simply a matter of programming. --}

type Square = Integer
type Cube = Integer

possible :: Square -> [Cube]
possible s = unfoldr next 0
    where
      ds = digits 10 s
      numDs = length ds
      next i | i == numDs = Nothing
             | (ds !! i) == 9 = next (i + 1) -- can't increment 9
             | otherwise = Just (inc i, i + 1)
      inc i = unDigits 10 $ take i ds ++ [(ds !! i) + 1] ++ drop (i + 1) ds

--Seems like there should be a way to use MonadPlus to let the caller decide whether one result (e.g. m = Maybe) or all results
--(e.g. m = []) are wanted. But I don't immediately see how to convert from [Cube] -> m Cube in a general way and don't have time
--to figure it out at the moment, so do the gross hack of jsut returning the first valid value.
cubeDatSquare :: MonadPlus m => Square -> m Cube
cubeDatSquare sqr = if not (null result) then return (head result) else mzero 
    where
      result = filter isCube $ possible sqr

-- so a square gozin and a cube comezout ... if the cube is the square with
-- just one of its digits incremented by 1; otherwise you get the big, fat
-- mzero. Find a 'sqr' that returns a cube that's (obviously) not mzero

-- *Main Data.Maybe> head $ filter (\(_, x) -> isJust x) $ map (\x -> (x^2, (cubeDatSquare $ x^2))) [1..]
-- (39204,Just 39304)
