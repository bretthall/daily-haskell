{--

101 Puzzles in Thought and Logic, problem 5:

Brown, Clark, Jones and Smith are the names of the men who hold, thought not
necessarily respectively, the position of accountant, cashier, manager, and
president in the First National Bank of Fairport.

1. Although the cashier beats him consistently, the president will play chess
   with no one else in the bank.
2. Both the manager and the cashier are better chess players than the 
   accountant.
3. Jones and Smith are nextdoor neighbors and frequently play chess together
   in the evening.
4. Clark plays a better game of chess than Jones.
5. The accountant lives near the president but not near any of the others.

WHAT POSITION DOES EACH MAN HOLD?

 --}

import Data.List (permutations, find)
import Data.Maybe (fromJust)

data Person = Brown | Clark | Jones | Smith deriving (Enum, Show, Eq)

people :: [Person]
people = enumFrom Brown

data Position = Accountant | Cashier | Manager | President deriving (Enum, Show, Eq)

positions :: [Position]
positions = enumFrom Accountant

type Men = (Person, Position)

possible :: [[Men]]
possible = map (zip people) $ permutations positions

holder :: Position -> [Men] -> Person
holder pos = fst.fromJust.(find $ (pos ==).snd)  

positionOf :: Person -> [Men] -> Position
positionOf person = snd.fromJust.(find $ (person ==).fst)  

-- OUT OF TIME FOR NOW

-- rule3 :: [Men] -> Bool
-- rule3 ms = (positionOf Jones ms) `elem` [


firstNationalBank :: [[Men]]
firstNationalBank = undefined