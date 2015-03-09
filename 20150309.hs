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

isFactorOf :: Int -> Int -> Bool
isFactorOf n m = m `mod` n == 0

factors :: Int -> [Int]
factors n = [i | i <- [1..n], i `isFactorOf` n]

--ages' product is 36
possible = [(s, d1, d2) |
             s <- factors 36,
             d1 <- factors (36 `div` s),
             let d2 = 36 `div` (s*d1)]

--ages sum to Iskandar's age and that isn't enough information (only keep Iskander ages that have more than one solution)
agesSum :: [(Int, [(Int, Int, Int)])]
agesSum = filter ((>1).length.snd) $ map sumOK [1..maxAge]
    where
      maxAge = maximum (map (\(x,y,z) -> x + y + z) possible)
      sumOK age = (age, [(x,y,z) | (x,y,z) <- possible, x + y + z == age])

--the son is more than 1 year older than the daughters
agesOK :: (Int, Int, Int) -> Bool
agesOK (s, d1, d2) = (s - 1) > d1 &&  (s - 1) > d2

okAges :: [(Int, [(Int, Int, Int)])]
okAges = filter (not.null.snd) $ map (\(age, as) -> (age, filter agesOK as)) agesSum

-- Iskander is "young"
youngEnough :: [(Int, [(Int, Int, Int)])]
youngEnough = filter (\(a, _) -> a < 18) okAges

--Now we're supposed to have enough information so restrict to Iskander ages that only have one solution
uniqueSolution :: [(Int, [(Int, Int, Int)])]
uniqueSolution  = filter ((==1).length.snd) youngEnough

kamarsChildrensAges = head $ snd $ head uniqueSolution

-- *Main> kamarsChildrensAges
-- (9,2,2)
