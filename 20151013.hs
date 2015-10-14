import Data.Array
import Data.Array.ST as DAST
import qualified Data.List as DL

type Matrix a = Array (Int, Int) a

fromLists :: [[a]] -> Matrix a
fromLists rs = listArray ((1, 1), (numRows, numCols)) $ cutAndConcat rs
    where
      numRows = length rs
      numCols = minimum $ map length rs
      cutAndConcat = foldr ((++) . take numCols) []

pprint :: Show a => Matrix a -> IO ()
pprint a = mapM_ (printRow a) [1..numRows]
    where
      (_, (numRows, numCols)) = bounds a
      printRow a r = print $ unwords $ map (show.(a!)) [(r, c) | c <- [1..numCols]] 


{--

So, we've now got a matrix that we can create and then view.

Very nice.

But what does one ... DO with it?

Well, the 'hello, world!' for matrices is the transpose-operation.

What is the transpose-operation, you ask.

Well, a good description of Data.Matrix.transpose can be found at 
Data.List.transpose. Okay read that.

You say: tl;dr.

Okay then:

transpose [[1,2,3]       == becomes ==>   [[1,4]
           [4,5,6]]                        [2,5]
                                           [3,6]]

Why is transpose important or useful, geophf?

My answer: wikipedia (aka: because same) https://en.wikipedia.org/wiki/Transpose

SO, now that we are forearmed with awesome knowledge, let us define transpose
for matrices.

Question: Data.List.transpose. Is it useful here, or not?

Question: As opposed to the laborious process of reconstructing the transposed
matrix by assembling the new transposed-array one element at a time, I see a 
linear-time definition here. Do you?
--}

transpose :: Matrix a -> Matrix a
--transpose = fromLists.cols -- too easy, let's do it in place:
transpose m = runSTArray $ do
  let ((_, _), (numRows, numCols)) = bounds m
  m' <- newArray_ ((1, 1), (numCols, numRows))
  mapM_ (\((r, c), e) -> writeArray m' (c, r) e) $ assocs m
  return m'
  
-- what is transpose tehMatrix?

tehMatrix :: Matrix Int
tehMatrix = fromLists [[1,2,3],[4,5,6]]

-- I mean, like, show your haskelly-answer to that question, please.
-- *Main> pprint $ transpose tehMatrix
-- "1 4"
-- "2 5"
-- "3 6"

{--
Bonus question: 

I see a constant-time definition here from the wikipedia article. Do you?

(but that involves some 'magic' with 'views into' the (transposed-)matrix)

Oh, and besides element-access for matrices, which we ('kinda-really') defined
yesterday ... http://lpaste.net/3386266226073272320 we also want definitions
for row-views and column-views into matrices. Define those.
--}

cell :: Matrix a -> (Int, Int) -> a
cell = (!)

-- what is the (2,2)-cell of tehMatrix?
-- *Main> cell tehMatrix (2,2)
-- 5

-- what is the (2,2)-cell of transpose tehMatrix?
-- *Main> cell (transpose tehMatrix) (2,2)
-- 5

rows, cols :: Matrix a -> [[a]]

rows m = map extractRow [1..numRows]
  where
    ((_, _), (numRows, numCols)) = bounds m
    extractRow r = map (cell m) [(r, c) | c <- [1..numCols]]

cols m = map extractCol [1..numCols]
  where
    ((_, _), (numRows, numCols)) = bounds m
    extractCol c = map (cell m) [(r, c) | r <- [1..numRows]]

-- what is rows tehMatrix?
-- what is cols tehMatrix?
-- what are they for transpose tehMatrix? What can be said about their relationship?
-- *Main> rows tehMatrix
-- [[1,2,3],[4,5,6]]
-- *Main> cols tehMatrix
-- [[1,4],[2,5],[3,6]]
-- *Main> rows $ transpose tehMatrix
-- [[1,4],[2,5],[3,6]]
-- *Main> cols $ transpose tehMatrix
-- [[1,2,3],[4,5,6]]
-- ==> transpose swaps rows and cols

row, col :: Int -> Matrix a -> [a]
row r m = (rows m) !! (r - 1)
col c m = (cols m) !! (c - 1)

-- what is row 2 for tehMatrix? ... for transpose tehMatrix?
-- *Main> row 2 tehMatrix
-- [4,5,6]
-- *Main> row 2 $ transpose tehMatrix
-- [2,5]

-- what is col 2 for tehMatrix? ... for transpose tehMatrix?
-- *Main> col 2 tehMatrix
-- [2,5]
-- *Main> col 2 $ transpose tehMatrix
-- [4,5,6]

-- hint: the definition for row and col functions may possible depend on the 
-- definitions for rows and cols functions.
