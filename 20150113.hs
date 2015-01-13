{--

Solution to @1HaskellADay http://lpaste.net/118367

posted as http://lpaste.net/118384

So, yesterday, I didn't publish a puzzle. Ick. Work. You know? My apologies.

My daughter suggested, "Well, how about two plus two?"

And I thought that maybe that was a bit too easy, but then I thought, how
about proving two plus two (major spoiler alert:) equals, well, four using
dependent types in Haskell, but then I fell asleep on that thought, so no
two plus two equals four is true math-logic problem.

Bummer.

Besides, how does one ... do that in Haskell?

So, today, I was rescued by twitter:

Benjamin Vitale ‏@BenVitale
A little fun #puzzle 

And the puzzle is, imagine two rows of four circles each, and in each circle
is a number, thus:


          1           2          7           9



          3           4          5           8


(you may pencil in the circles if you wish)

The question is:

"Show how to make the sums of the two rows equal 
 by moving just two of the pieces"

Ooh! So, there you have it! Today's haskell puzzler! --}

import Data.List (permutations)

rotations :: [a] -> [[a]]
rotations as = go num cs
    where
      num = length as
      cs = cycle as
      go n l@(b:bs) | n == 0 = []
                    | otherwise = take num l: go (n - 1) bs

good :: Int -> [Int] -> [Int] -> Bool
good n r1 r2 = sum (take n r1 ++ drop n r2) == sum (take n r2 ++ drop n r1)

sumsEqual :: Int -> [Int] -> [Int] -> ([Int], [Int])
sumsEqual numSwaps inputRow1 inputRow2 = go rots1 perms2
    where
      rots1 = rotations inputRow1
      -- if we were only doing one swap between rows then we could just do all the rotations of one row against the other, but
      -- once we start swapping more than one set of elements we need to do permutations of one of the rows
      perms2 = permutations inputRow2
      go (r1:r1s) l2@(r2:r2s) = if good numSwaps r1 r2 
                                then (r1, r2)
                                else go r1s l2
      go [] (_:r2s) = go rots1 r2s
      go _ [] = ([], [])

-- *Main> map (\n -> sumsEqual n [1,2,7,9] [3,4,5,8]) [1..3]
-- [([],[]),([],[]),([],[])]
-- => Either this has no solution or I don't understand the question
-- Does "moving two pieces" mean swapping a number from one row with a number from the other row?