import Data.Digits
import Data.List
import Data.Numbers.Primes
import Control.Monad

{--
Solution to @1HaskellADay http://lpaste.net/121154

James Tanton @jamestanton  9m9 minutes ago
113 has the prop all arrangements of its digits (113,131,311) are prime: Same 
true of 199. Another 3-digit eg? A 4-digit eg? 5-digit?

--}

type NumDigits = Int
type Digits = [Int]
type Prime = Int

circPrimes :: MonadPlus m => NumDigits -> m Prime
circPrimes ndigits = msum $ map isGoodPrime nDigitsPrimes
    where
      nDigitsPrimes = takeWhile (\ds -> length ds == ndigits) $ dropWhile (\ds -> length ds < ndigits) $ map (digits 10) primes

-- Note: using permutations below since the problem says "arrangements", not "rotations". It turns out not to matter,
-- the only valid primes are either two digits where the set of rotations equals the set of permutations or are three 
-- digits numbers with a repeated digit in which case the rotations and permutations are again equal. There are no 4 
-- or 5 digits primes that fit the bill. 
isGoodPrime :: MonadPlus m => Digits -> m Prime
isGoodPrime ds = if all isPrime $ map (unDigits 10) $ permutations ds
                 then return $ unDigits 10 ds
                 else mzero

-- *Main> (circPrimes 2)::[Prime]
-- [11,13,17,31,37,71,73,79,97]
-- *Main> (circPrimes 3)::[Prime]
-- [113,131,199,311,337,373,733,919,991]
-- *Main> (circPrimes 4)::[Prime]
-- []
-- *Main> (circPrimes 5)::[Prime]
-- []
