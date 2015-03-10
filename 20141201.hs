import Control.Monad
import Data.List (foldl')
{--

This is from the twitter-verse:

21h:  @SWPhantom @glipsman @numberdotten @CompSciFact Neither. Lambda terms 
let you build both (or anything) from composition. #deeperthoughts

So, the question was, if you were to chose one, which would it be: for-loops
or if-statements.

(I know; I know!)

My answer was the above: neither. Using lambda-terms you get both, and anything
else you want thrown in there, too.

So, let's put our money where our mouths are, eh?

1. Using lambda terms model a for-loop:

--}

forLoop :: Int -> Int -> (Int -> a -> a) -> a -> a
forLoop from to fn seed = foldl' (flip fn) seed [from..to]

{--

Okay, so I KNOW it's a fold, but, so, okay, use a fold if you'd like.

using the forLoop above, have it do something like the below pseudocode below
does:

10 FOR I = 1 TO 10
20 PRINT I
30 NEXT I
40 END

... you don't need to model the 'END,' ... unless you want to do that for
extra credit.

--}

printVal :: Int -> [IO ()] -> [IO ()]
printVal n = (++ [print n])

-- *Main> sequence_ $ forLoop 0 5 printVal []
-- 0
-- 1
-- 2
-- 3
-- 4
-- 5


{--

2. Now, using lambda terms, model the if-statement, and, yes, I know Haskell
has if-syntax already, so that makes modelling the if-statement incredibly
hard, but go ahead and do that.

(you may find introductory papers on combinator logic (the SK-basis) to be
helpful here)

--}

k :: a -> b -> a
k x _ = x

s :: (a -> b -> c) -> (a -> b) -> a -> c
s x y z = x z (y z)

i :: a -> a
i = s k k

t :: a -> b -> a
t = k

-- f :: a -> b -> b
-- f = s k
-- Hmm, that doesn't work and I don't have time to go 0 to 60 on SK calculus
-- right now so let's just do the easy thing:
ifThen :: MonadPlus m => Bool -> a -> m a
ifThen True ans = return ans
ifThen False _ = mzero

unless_ :: MonadPlus m => Bool -> a -> m a
unless_ False ans = return ans
unless_ True _ = mzero

ifThenElse :: Bool -> a -> b -> Either a b
ifThenElse True ans _ = Left ans
ifThenElse False _ ans = Right ans
