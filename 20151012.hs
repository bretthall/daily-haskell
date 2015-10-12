module Data.Matrix where

import Data.Array
import Data.List

{--

Hey, Haskellers!

So 'yesterday'! We constructed a matrix from an array of arrays of things, but
is that really the way to go about doing things?

Why do you ask, geophf?

Thank you for that lead-in.

The thing is with arrays is that they are indexed by an Ix ('indexible')-type
and an indexible type is any type that can be used as an index (usually being an
Enum(erable)-type is a Good Thing(tm)).

Well, what's to stop our indexible-type from being a (2-)tuple of Ints?

What, indeed?

Why do you say all that, geophf?

Thank you. I say all that, because, in normal matrix-definitions (one of which
you saw 'yesterday') with Array-of-Arrays, indexing to a specific element 
requires a bit of tedium on our part to calculate which array and which element
of that array is the matrix-element we are looking for.

And I still.
Haven't found.
What I'm lookin' for.

BUT if we redeclare our ho-hum matrices to be SUPAH-MATRICES of one Array
indexed by a 2-tuple type, then finding the element we are looking for becomes
child's-play, as we hand off to the Array's indexing functions to handle those
pesky details for us.

Let's do that.

Redeclare Matrix a to be an array-type indexed by a 2-tuple type. --}

type Matrix a = Array (Int, Int) a

-- define a (function-)type fromLists that takes a list-of-lists and returns
-- a matrix of those same dimensions. What happens if they lists are not all of
-- the same length?
-- => Cut-off rows that are too long
fromLists :: [[a]] -> Matrix a
fromLists rs = listArray ((1, 1), (numRows, numCols)) $ cutAndConcat rs
    where
      numRows = length rs
      numCols = minimum $ map length rs
      cutAndConcat (r:rs) = take numCols r ++ cutAndConcat rs
      cutAndConcat [] = []
                        
-- redefine pprint so it works with this new type pretty-printing as before

pprint :: Show a => Matrix a -> IO ()
pprint a = mapM_ (printRow a) [1..numRows]
    where
      (_, (numRows, numCols)) = bounds a
      printRow a r = print $ concat $ intersperse " " $ map (show.(a!)) [(r, c) | c <- [1..numCols]] 

-- pprint the following matrix:

ourMatrix :: Matrix Int
ourMatrix = fromLists (take 5 (zipWith enumFromTo [1,6..] [5,10..]))

-- *Data.Matrix> pprint ourMatrix
-- "1 2 3 4 5"
-- "6 7 8 9 10"
-- "11 12 13 14 15"
-- "16 17 18 19 20"
-- "21 22 23 24 25"

-- What is the (3,4)-element of ourMatrix

threeDownLowOnThe4 :: (Int, Int) -> Matrix a -> a
threeDownLowOnThe4 = flip (!)

-- *Data.Matrix> threeDownLowOnThe4 (3,4) ourMatrix
-- 14
