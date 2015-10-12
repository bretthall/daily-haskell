{--

Suggested by ...

   Alexey Radkov @sheshanaag
   @geophf Have you ever asked impossible puzzle 
   http://en.wikipedia.org/wiki/Sum_and_Product_Puzzle in 1HD? 
   I have a nice solution for that :)

... so, YES, you, YES, YOU, can suggest a @1HaskellADay puzzler for the day! :)

X and Y are two different integers, greater than 1, with sum less than or equal
to 100. S and P are two mathematicians; S knows the sum X+Y, P knows the 
product X*Y, and both are perfect logicians. Both S and P know the information 
in these two sentences. The following conversation occurs:

S says "P does not know X and Y."
P says "Now I know X and Y."
S says "Now I also know X and Y!"
What are X and Y?

So, leading questions:

What are the cases in which the sum of x and y are ambiguous? but the product
of that ambiguous set is NOT. Ahhhhh! AND, given that x and y are ambiguous
with different products, once a product is settled upon, how does the two
numbers become settled?

Write a Haskell program that does all this hard thinking for you!

Please. --}

import qualified Data.Map.Strict as M

uniqueXY :: Int -> Int -> [(Int, Int)]
uniqueXY sumIs productIs = undefined

type Pair = (Int, Int)

pairs :: [Pair]
pairs = [(x, y) | x <- [2..50], y <- [x..(100 - x)]]

type Result = (Int, Int, Int)

pairProduct :: Pair -> Result
pairProduct (x, y) = (x*y, x, y)

pairSum :: Pair -> Result
pairSum (x,y) = (x+y, x, y)

type SameResult = M.Map Int [Pair]

same :: [Result] -> SameResult
same ps = same M.empty ps
    where
      same m [] = m
      same m ((p, x, y):ps) = same (M.insertWith (++) p [(x,y)] m) ps

type SamePair = M.Map Pair [Int]

invert :: SameResult -> SamePair
invert r = invert' M.empty $ M.toList r
    where
      invert' m [] = m
      invert' m ((r, ps):rs) = invert' (update r ps m) rs
      update r [] m = m
      update r (p:ps) m = update r ps (M.insertWith (++) p [r] m)

sameSums :: SameResult
sameSums = same $ map pairSum pairs

sameProducts :: SameResult
sameProducts = same $ map pairProduct pairs

ambiguous :: SameResult -> [(Int, [Pair])]
ambiguous = (filter ((> 1).length.snd)) . M.toList



-- unique :: Same -> [Int]
-- unique = (map fst) . (filter ((== 1).length.snd)) . M.toList
