-- Solution to @1HaskellADay http://lpaste.net/2775082411233378304

-- Just going to cut-n-paste the matrix stuff from the past few days to make it easire to look at in lpaste

import Data.Array
import Data.Array.ST

--See the end of the file for the matrix implementation

{--

Okay, so now that we've got row, col, and cell defined, what more could we
possibly want from matrices?

Multiplication, of course.

Okay, here we go.

Matrix-multiplication is defined as 

https://en.wikipedia.org/wiki/Matrix_multiplication

(Yes, a wikipedia article. Yes. I went there.)

Or, tl;dr    A(nxm) x B(mxp) = C(nxp) where each element in C is
defined as Cxy = (AxBy). (that is: the sum of the products of A's row x
and B's column y)

Let's look at the example where we want to buy beef, chicken and veggie-pies

https://www.mathsisfun.com/algebra/matrix-multiplying.html

If we have this matrix of pies that were sold

              Mon   Tues   Wed   Thur
Beef           13     9     7     15 
chicken         8     7     4      6
veggies         6     4     0      3

And their values are

Beef pie:    $3
Chicken pie: $4
Veggie pie:  $2

Then we can calculate how much we made using matrix multiplication:

[ 3 4 2 ]  x  (the matrix) = sales for each day.

Let's do this.

--}

-- determines whether two matrices can be multiplied (in the order given) or not
areCrossCompatible :: Matrix a -> Matrix a -> Bool
areCrossCompatible m1 m2 = numCols m1 == numRows m2

-- Multiplies the two matrices, does no bounds checking, you should use cross or crossMaybe instead
unsafeCross :: Num a => Matrix a -> Matrix a -> Matrix a
-- the (i,j) element of the product is the inner product of the i'th row from m1 with the j'th column of m2
-- probably faster to use runSTArray, but that would be a lot more (imperative) code
unsafeCross m1 m2 = fromLists $ map (\r -> map (\c -> sum $ zipWith (*) (row r m1) (col c m2)) [1..numCols m2]) [1..numRows m1]

--multiplies the two matrices, calls error if they're not compatible
cross :: Num a => Matrix a -> Matrix a -> Matrix a
cross m1 m2 = if areCrossCompatible m1 m2
              then unsafeCross m1 m2
              else error "Incompatible matrices multiplied"

-- multiplies the two matrices, yeilds Nothing if they're not compatible
crossMaybe :: Num a => Matrix a -> Matrix a -> Maybe (Matrix a)
crossMaybe m1 m2 = if areCrossCompatible m1 m2
                   then Just $ unsafeCross m1 m2
                   else Nothing

-- when you've defined the product function, multiply the above two matrices
-- (provided below). What is your result?

prices, sales :: Matrix Int
prices = fromLists [[3,4,2]]

sales = fromLists [[13,9,7,15],
                   [ 8,7,4, 6],
                   [ 6,4,0, 3]]

-- *Main> pprint $ cross prices sales
-- "83 63 37 75"

{--

A consideration

5 x 3 always works

(broad statement, but let's go with it)

But Ma x Mb doesn't always work, particularly if either is 
dimensionally-challenged ... how should we handle this type-wise?

Ma x Mb = Mabye Mc ???

So Matrix-multiplication is monadic then? Is that a Good Thing(tm)?

Or should 

Ma x Mb = error 

and we explode the universe because we attempted to multiply incompatible
matrices.

Thoughts? Discuss.
--}

-- It was easy enough to provide both, so why not provide both. We also get an "unsafe" version
-- that does no bounds checking for those who want to live on the edge.


--------------------------------------------------------------------------------
-- Matrix implementation from the past few days

type Matrix a = Array (Int, Int) a

fromLists :: [[a]] -> Matrix a
fromLists rs = listArray ((1, 1), (numRows, numCols)) $ cutAndConcat rs
    where
      numRows = length rs
      numCols = minimum $ map length rs
      cutAndConcat = foldr ((++) . take numCols) []

dims :: Matrix a -> (Int, Int)
dims m = (numRows, numCols)
  where
      (_, (numRows, numCols)) = bounds m

numRows, numCols :: Matrix a -> Int
numRows = fst.dims 
numCols = snd.dims

pprint :: Show a => Matrix a -> IO ()
pprint a = mapM_ (printRow a) [1..numRows]
    where
      (numRows, numCols) = dims a
      printRow a r = print $ unwords $ map (show.(a!)) [(r, c) | c <- [1..numCols]] 

transpose :: Matrix a -> Matrix a
--transpose = fromLists.cols -- too easy, let's do it in place:
transpose m = runSTArray $ do
  let (numRows, numCols) = dims m
  m' <- newArray_ ((1, 1), (numCols, numRows))
  mapM_ (\((r, c), e) -> writeArray m' (c, r) e) $ assocs m
  return m'

cell :: Matrix a -> (Int, Int) -> a
cell = (!)

rows, cols :: Matrix a -> [[a]]

rows m = map extractRow [1..numRows]
  where
    (numRows, numCols) = dims m
    extractRow r = map (cell m) [(r, c) | c <- [1..numCols]]

cols m = map extractCol [1..numCols]
  where
    (numRows, numCols) = dims m
    extractCol c = map (cell m) [(r, c) | r <- [1..numRows]]

row, col :: Int -> Matrix a -> [a]
row r m = rows m !! (r - 1)
col c m = cols m !! (c - 1)
