{--

Solution to @1HaskellADay 
From 101 Puzzles in Thought and Logic by C.R. Wylie, Jr. Problem 7:

Brown, Clark, Jones and Smith are four substantial citizens who serve their
community as architect, banker, doctor and lawyer, though not necessarily
respectively.

* Brown, who is more conservative than Jones but more liberal than Smith, is a
  better golfer than the men who are older than he is and has a larger income
  than the men who are younger than Clark

* The banker, who earns more than the architect, is neither yonugest nor the
  oldest.

* The doctor, who is a poorer golfer than the lawyer, is less conservative than
  the architect.

* As might be expected, the oldest man is the most conservative and has the
  largest income, and the youngest man is the best golfer.

WHAT IS EACH MAN'S PROFESSION --}

module Main where

import Data.List
import Control.Arrow ((&&&))
import Control.Parallel.Strategies
import Data.Maybe (isJust, fromJust)
import Rules -- http://lpaste.net/114590

data Name = Brown | Clark | Jones | Smith
   deriving (Eq, Ord, Enum, Bounded, Show, Read)

instance NFData Name

data Profession = Architect | Banker | Doctor | Lawyer
   deriving (Eq, Ord, Enum, Bounded, Show, Read)

instance NFData Profession

data Conservatism = Con Int deriving (Show, Eq , Ord)
instance NFData Conservatism

data GolfAbility = Golf Int deriving (Show, Eq, Ord)
instance NFData GolfAbility

data Age = Age Int deriving (Show, Eq, Ord)
instance NFData Age

data Income = Income Int deriving (Show, Eq, Ord)
instance NFData Income

enumerate :: (Int -> b) -> [b]
enumerate = flip map [1..4]

data Person = Person {name::Name, prof::Profession, con::Conservatism, golf::GolfAbility, age::Age, income::Income}
            deriving (Show)
instance NFData Person

type People = [Person]

people :: [Name] -> [Profession] -> [Conservatism] -> [GolfAbility] -> [Age] -> [Income] -> People
people = zipWith6 Person

possible :: [People]
possible = [people [minBound::Name ..] profs cons golfs ages incomes |
            profs <- permutations [minBound::Profession ..],
            cons <- permutations $ enumerate Con,
            golfs <- permutations $ enumerate Golf,
            ages <- permutations $ enumerate Age,
            incomes <- permutations $ enumerate Income]

type Rule = People -> Bool

brownConservative :: Rule
brownConservative [b, c, j, s] = con b > con j && con b < con s

brownGolfer :: Rule
brownGolfer (b:rest) = all ((golf b >).golf) $ filter ((age b <).age) rest

findPerson :: Eq a => (Person -> a) -> a -> [Person] -> Person
findPerson val a = fromJust.find ((== a).val)

brownIncome :: Rule
brownIncome (b:rest) = all ((income b >).income) $ filter ((age c <).age) rest
    where 
      c = findPerson name Clark rest

bankerIncome :: Rule
bankerIncome ps = income banker > income architect
    where 
      banker = findPerson prof Banker ps
      architect = findPerson prof Architect ps

bankerAge :: Rule
bankerAge ps = bankerAge > Age 1 && bankerAge < Age 4
    where 
      bankerAge = age $ findPerson prof Banker ps

doctorGolf :: Rule
doctorGolf ps = golf doctor < golf lawyer
    where 
      doctor = findPerson prof Doctor ps
      lawyer = findPerson prof Lawyer ps

doctorConservative :: Rule
doctorConservative ps = con doctor < con architect
    where 
      doctor = findPerson prof Doctor ps
      architect = findPerson prof Architect ps

oldest :: Rule
oldest ps = con old == Con 4 && income old == Income 4
    where 
      old = findPerson age (Age 4) ps

youngest :: Rule
youngest ps = golf young == Golf 4
    where 
      young = findPerson age (Age 1) ps

rules :: [Rule]
rules  = [brownConservative, brownGolfer, brownIncome, bankerIncome, bankerAge, doctorGolf, doctorConservative, oldest, youngest]

peeps :: [People]
peeps = filter (satisfiesAll rules) possible

justGuessing :: [[(Name, Profession)]]
justGuessing = nub $ map (map (name &&& prof)) peeps

-- *Main> justGuessing
-- [[(Brown,Banker),(Clark,Lawyer),(Jones,Doctor),(Smith,Architect)]<interactive>: out of memory

--OOPS, this is beyond ghci's capabilities
--enlist ghc instead:
-- ghc -O2 20141203.hs

main = mapM_ print justGuessing

-- bhall $./20141203.exe
-- [(Brown,Banker),(Clark,Lawyer),(Jones,Doctor),(Smith,Architect)]
-- [(Brown,Architect),(Clark,Lawyer),(Jones,Doctor),(Smith,Banker)]

--For the heck of it lets parallelize it:
rulesApplied :: [Maybe People]
rulesApplied = map (\p -> if satisfiesAll rules p then Just p else Nothing) possible `using` parListChunk 5000 rdeepseq
-- 5000 was found by trial and error, it seems to provide the best speed-up, note that parList causes memory exhaustion

peepsParallel :: [People]
peepsParallel = map fromJust $ filter isJust rulesApplied

-- to run parallel replace "peeps" in justGuessing by "peepsParallel" then
-- ghc -threaded -O2 -rtsopts 20141203.hs

-- this gets a speed-up factor of a little more than 2 going from -N1 to -N4, but -N4 is still only half as fast
-- as the non-parallel version, in threadscope I'm seeing a lot of GC going on, don't have enough time to dig into 
-- this deeper right now, probably need to use the Par monad to parallize the filtering in a more efficient manner