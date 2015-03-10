import Data.List
import Control.Monad

{-- 
Solution to @1HaskellADay http://lpaste.net/120775

Okay, here's a fun little problem from Mensa's Genius Quiz-a-Day Book:

February 16th, 2015:

How many common English words can you make from the letters AEKL? (All the
letters must be used for each word).

(ed: And all your base are belong to us, too, for that matter.)

--}

type Word = String

aekl :: FilePath -> IO [Word]
aekl dict = liftM (filter isOK . words) $ readFile dict
  
isOK :: Word -> Bool
isOK w = length w >= 4 && (sort.nub) w == "aekl"

-- hint: At least how many letters are required to be in the solution set? So
-- all those words of one, two, ... etc, ... letters length ... do you really
-- need to scan those or can you, ehem, sieve them out? Eh? Eh?

{--
*Main> aekl "5desk.txt"
["kale","lake","leak"]
--}