import Data.Set (Set)
import qualified Data.Set as Set

--import Control.Reasoning.Ontological      -- http://lpaste.net/111863

{--

From 101 Puzzles in Thought and Logic by C.R. Wylie, Jr., Problem 3:

Dorothy, Jean, Virginia, Bill, Jim and Tom are six young persons who have been
close friends from their childhood.  They went through high school and college 
together, and when they finally paired off and became engaged nothing would do
but a triple announcement party. Naturally, they wanted to break the news to
their friends in an unusual fashion, and after some thought they decided upon
this scheme.

At just the right moment during the party, everyone was given a card bearing
the cryptic information:

	Who now are six will soon be three,
		And gaily we confess it.
	But how we've chosen you may know
		No soon than you guess it.

* Tom, who is older than Jim, is Dorothy's brother.
* Virginia is the oldest girl.
* The total age of each couple-to-be is the same although no two of us are
  the same age.
* Jim and Jean are together as old as Bill and Dorothy.

WHAT THREE ENGAGEMENTS WERE ANNOUNCED AT THE PARTY? --}

data Boy = Bill | Jim | Tom
   deriving (Eq, Ord, Enum, Show, Read)
data Girl = Dorothy | Jean | Virginia
   deriving (Eq, Ord, Enum, Show, Read)

boys = enumFrom Bill
girls = enumFrom Dorothy

pairings :: [[(Boy, Girl)]]
pairings = pairingsEven boys (cycled girls)
    where
      pairingsEven _ (_:_:[]) = pairingsOdd boys (cycled $ odded girls)
      pairingsEven bs gs = (zip bs gs) : (pairingsEven bs $ tail gs)
      pairingsOdd _ (_:_:[]) = []
      pairingsOdd bs gs = (zip bs gs) : (pairingsOdd bs $ tail gs)
      cycled = (take 5).cycle
      odded (x1:x2:xs) = x2:x1:xs

removeSiblings :: [[(Boy, Girl)]] -> [[(Boy, Girl)]]
removeSiblings = filter ((Tom, Dorothy) `notElem`)  

--data Straight = Engaged

--type Engagement = Relation Boy Girl Straight

--rule1 b g = (b /= Tom) or (g /= Dorothy)

-- age Tom > age Jim
-- (age Virginia > age Dorothy) and (age Virginia > age Jean)
-- (age Jim + age Jean) == (age Bill + age Dorothy)

-- announced :: [Boy] -> [Girl] -> Set (Set Engagement)
-- announced snipsSnailsPuppyDogTails sugarSpiceEverythingNice = undefined
