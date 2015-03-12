{--

Solution to @1HaskellADay http://lpaste.net/114559

From the Mensa Genius Quiz-a-Day book, October 18th:

Of the three finalists in the science scholarship contest, John, William, and
Sally came in one, two, and three, but not necessarily in that order. The
winner was a physicist. The one who was not last or first was the
mathematician. The one who came in last had black hair. John had brown hair,
Sally had red hair. John did not have any training in physics. Who was first?

--}

import Data.List (zipWith4, permutations, find, sortBy)
import Data.Maybe (fromJust)
import Data.Function (on)
import Control.Arrow ((&&&))
import Rules

data Person = John | William | Sally
              deriving (Eq, Ord, Enum, Show, Read)

persons :: [Person]
persons = enumFrom John

data Place = First | Second | Last
             deriving (Eq, Ord, Enum, Show, Read)

places :: [Place]
places = enumFrom First

-- "... all else is stamp collecting" (being charitable to the mathemeticians here)
data Training = Physicist | Mathematician | StampCollector
                deriving (Eq, Ord, Enum, Show, Read)

trainings :: [Training]
trainings = enumFrom Physicist

data Hair = Black | Red | Brown
            deriving (Eq, Ord, Enum, Show, Read)

hairs :: [Hair]
hairs = enumFrom Black

data PersonInfo = PersonInfo {person :: Person,
                              place :: Place,
                              training :: Training,
                              hair :: Hair}
                  deriving (Show)

type Result = [PersonInfo]

resultFrom :: [Person] -> [Place] -> [Training] -> [Hair] -> Result
resultFrom = zipWith4 PersonInfo 

possiblities :: [Result]
possiblities = [resultFrom persons ps ts hs | 
                ps <- permutations places, 
                ts <- permutations trainings,
                hs <- permutations hairs]                                          

type Rule = Result -> Bool

hairIsCorrect :: Rule
hairIsCorrect [j, _, s] = (hair j == Brown) && (hair s == Red)

johnNotPhysicist :: Rule
johnNotPhysicist [j, _, _] = training j /= Physicist

findPlace :: Place -> Result -> PersonInfo
findPlace p = fromJust.find ((== p).place)

physicistWins :: Rule
physicistWins = (== Physicist).training.findPlace First

mathemeticianSecond :: Rule
mathemeticianSecond = (== Mathematician).training.findPlace Second

blackHairLast :: Rule
blackHairLast = (== Black).hair.findPlace Last

rules :: [Rule]
rules = [hairIsCorrect, johnNotPhysicist, physicistWins, mathemeticianSecond, blackHairLast]

okResults :: [Result]
okResults = filter (satisfiesAll rules) possiblities

-- *Main> length okResults
-- 1
-- => problem is well specified

-- *Main> head okResults
-- [PersonInfo {person = John, place = Second, training = Mathematician, hair = Brown},
--  PersonInfo {person = William, place = Last, training = StampCollector, hair = Black},
--  PersonInfo {person = Sally, place = First, training = Physicist, hair = Red}]
-- => All rules satisfied

scienceContest :: [(Person, Place)]
scienceContest = sortBy (compare `on` snd) $ map (person &&& place) $ head okResults

-- *Main> scienceContest
-- [(Sally,First),(John,Second),(William,Last)]
