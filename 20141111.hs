{--
Solution to 1HaskellADay http://lpaste.net/114089

Coin sums
Project Euler, Problem 31

In England the currency is made up of pound, £, and pence, p, and there 
are eight coins in general circulation:

1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
It is possible to make £2 in the following way:

1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
How many different ways can £2 be made using any number of coins?

--}

type Pence = Int

target :: Pence
target = 200

coinValues :: [Pence]
coinValues = [1, 2, 5, 10, 100, 200]

type NumCoins = Int
type Coins = [NumCoins]

total :: Coins -> Pence
total = sum.(zipWith (*) coinValues)

hitsTarget :: Coins -> Bool
hitsTarget = (== target).total

possiblities :: [Coins]
possiblities = [[c1, c2, c5, c10, c100, c200] |
                c1 <- [0..target],
                c2 <- [0..(newTarget [c1] 2)],
                c5 <- [0..(newTarget [c1, c2] 5)],
                c10 <- [0..(newTarget [c1, c2, c5] 10)],
                c100 <- [0..(newTarget [c1, c2, c5, c10] 100)],
                c200 <- [0..(newTarget [c1, c2, c5, c10, c100] 200)],
                hitsTarget [c1, c2, c5, c10, c100, c200]]
    where
      newTarget coins coinValue = max 0 $ (target - (total coins)) `div` coinValue

coinSumsFor2P :: Int
coinSumsFor2P = length possiblities

-- *Main> coinSumsFor2P
-- 17369
