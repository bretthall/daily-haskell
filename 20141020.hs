import Data.List (permutations, sortBy, findIndex)
import Data.Maybe (fromJust)

{--

This problem comes from 101 Puzzles in Thought and Logic, by C.J. Wylie, jr.

Mr.  Carter, Mr. Flynn, Mr. Milne, and Mr. Savage serve the little town of
Milford [ed: CT?] as architect, banker, druggist, and grocer, though not
necessarily respectively [ed: Oh, that there were a problem were the 
participants worked at their respective jobs! That's the problem with this
World, I think: the lack of respect in it. But I digress, as I am wont to do].
Each man's income is a whole number of dollars.

* The druggist earns exactly twice as much as the grocer
* The architect earns exactly twice as much as the druggist.
* The banker earns exactly twice as much as the architect.

Although Mr. Carter does not make more money than Mr. Flynn, Mr. Flynn does
not make twice as Mr. Carter.

Mr. Savage earns exactly $3776 more than Mr. Milne.

WHAT IS EACH MAN'S OCCUPATION?

--}


data Person = Carter | Flynn | Milne | Savage
   deriving (Eq, Ord, Enum, Show, Read)
data Occupation = Architect | Banker | Druggist | Grocer
   deriving (Eq, Ord, Enum, Show, Read)

pay :: Occupation -> Int
pay Architect = 4
pay Banker = 8
pay Druggist = 2
pay Grocer = 1

-- pay :: Occupation -> Occupation -> Ratio Int
-- pay o1 o2 = (payOverGrocer o1) % (payOverGrocer o2)

-- If a man does not have an occupation, then what is he? Not a man.

type Man = (Person, Occupation)

possibles :: [Person] -> [Occupation] -> [[Man]]
possibles ps os = map (zip ps) $ permutations os

sortOnPay :: [Man] -> [Man]
sortOnPay = sortBy (\m1 m2 -> compare (pay $ snd m1) (pay $ snd m2)) 

findPerson :: Person -> [Person] -> Int
findPerson p ps = fromJust $ findIndex (== p) ps

test :: [Man] -> Bool
test ms = and $ map (\t -> t ps) [t1, t2, t3]
    where
      ps = map fst ms

--Note that if two people are next to each other in the list that is sorted on pay
--the later entry in the list makes double that of the predecessor

--"Although Mr. Carter does not make more money than Mr. Flynn"
t1 :: [Person] -> Bool
t1 ps = (findPerson Carter ps) < (findPerson Flynn ps)

--"Mr. Flynn does not make twice as Mr. Carter"
t2 :: [Person] -> Bool
t2 ps = (findPerson Carter ps) /= ((findPerson Flynn ps) - 1)

--"Mr. Savage earns exactly $3776 more than Mr. Milne" plus 
--"Each man's income is a whole number of dollars" and some algebra shows that 
--Savage must come right after Milne in the list
t3 :: [Person] -> Bool
t3 ps = (findPerson Savage ps) == ((findPerson Milne ps) + 1)

-- An' all 'at, An' all 'at; still a man's a man, for a' 'at.


milford :: [Person] -> [Occupation] -> [[Man]]
milford people jobs = filter test $ map sortOnPay $ possibles people jobs

okJobs :: [[Man]]
okJobs = milford (enumFrom Carter) (enumFrom Architect)

-- *Main Data.List>okJobs
-- [[(Carter,Grocer),(Milne,Druggist),(Savage,Architect),(Flynn,Banker)]]

