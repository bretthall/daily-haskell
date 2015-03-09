{--
Solution to @!HaskellADay http://lpaste.net/8990755663210610688 by @bretthall

From the Riddle of Scheherazade, Chapter XI: metapuzzles

115 )) What are their ages?

"Iskandar was an extremely intelligent person [young boy] who once asked his
friend Kamar the ages in years of his three children. The following conversation
ensued:

Kamar:		The produce of their ages is thirty-six.
Iskandar:	That doesn't tell me their ages
Kamar:		Well, by coincidence, the sum of their ages is your own age.
Iskandar (after several minutes of thought):
		I still don't have enough information
Kamar:		Well, if this will help, my son is more than a year older
		than both his sisters.
Iskandar:	Oh, good! Now I know their ages.

"What are their ages?"

-----

These Scheherazade puzzles are in the theme of metalogic puzzles and coercive
logic puzzles, meaning that information is pushed/forced to a solution from
potentionally otherwise paradoxical information. This is a warming up
metapuzzle to get the coercive logic muscles toned. Have at it! On y va! --}

import Data.List

isFactorOf :: Int -> Int -> Bool
isFactorOf n m = m `mod` n == 0

factors :: Int -> [Int]
factors n = [i | i <- [1..n], i `isFactorOf` n]

possible :: [(Int, Int, Int)]
possible =  [(s, d1, d2) | 
             -- the kids' ages multiply to 36 => they're all factors of 36
             s <- factors36, 
             -- The daughters' ages are all more then 1 less than the son's age
             d1 <- filter (`isFactorOf` (36 `div` s)) $ takeWhile (< (s - 1)) factors36,
             d2 <- [36 `div` s `div` d1],
             -- remove redundant solutions
             d2 <= d1
            ]
    where
      factors36 = factors 36

--minimize Iskander's age to get a unique solution since Iskander is "young"
kamarsChildrensAges :: (Int, Int, Int)
kamarsChildrensAges = minimumBy minIskanderAge possible
    where
      minIskanderAge (s1, d11, d21) (s2, d12, d22) = compare (s1 + d11 + d21) (s2 + d12 + d22) 
