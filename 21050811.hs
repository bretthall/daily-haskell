import Control.Applicative

--Solution to @1HaskellADay http://lpaste.net/793067287459397632

{--

D00Dz!

So, yesterday, after reading the paper at 
http://www.cicm-conference.org/2015/fm4m/FMM_2015_paper_6.pdf

We figured out how to prove the divisible-by-3 rule in Haskell.

Today, we're going to see how divisible-by rules compose.

So, prove the following without using the mod-function: 

--}

divby3 :: Int -> Bool
divby3 x | x < 0 = divby3 (-x) -- sign doesn't matter here and digits only works for positive numbers
         | s `elem` [3,6,9]  = True
         | otherwise = False
         where
           s = sums x
           sums n | length ds == 1 = head ds
                  | otherwise = sums $ sum ds
                  where
                    ds = digits n

digits :: Int -> [Int]
digits = map (read.(:"")) . show

divby2, divby5 :: Int -> Bool
divby2 = (`elem` [0,2,4,6,8]).last.digits
divby5 = (`elem` [0,5]).last.digits

-- Okay, NOW, using divby2 and divby5 define divby10:

divby10 :: Int -> Bool
divby10 n = divby5 n && divby2 n

-- OKAY, NOOOOOOOW, using your defintion of divby3 from yesterday
-- http://lpaste.net/5543352342910337024 and these definitions here, 
-- define divby30 

divby30 :: Int -> Bool
divby30 n = divby10 n && divby3 n

--applicative instance so that coldfiltered looks "elegant"
newtype FourTup a = FourTup { getTup :: (a, a, a, a) } deriving (Show, Eq, Ord)

instance Functor FourTup where
    fmap f (FourTup (a1, a2, a3, a4)) = FourTup (f a1, f a2, f a3, f a4)

instance Applicative FourTup where
    pure a = FourTup (a, a, a, a)
    FourTup (f1, f2, f3, f4) <*> FourTup (a1, a2, a3, a4) = FourTup (f1 a1, f2 a2, f3 a3, f4 a4)

-- filter the numbers below in nums into divby-2,5,10,and 30 buckets using
-- your divby functions defined above

coldfiltered :: [Int] -> ([Int], [Int], [Int], [Int])
coldfiltered ns = getTup $ pure filter <*> FourTup (divby2, divby5, divby10, divby30) <*> pure ns

nums :: [Int]         -- numbers generated from random.org
nums = [89163,50965,77963,81908,95285,
        42956,51050,96353,22667,21149,
        88276,46830,28368,66370,73908,
        30545,32368,24079,5100,81055,
        80207,53050,6304,71343,54400,
        94638,47074,71977,21280,23525,
        77393,49562,86028,18542,16189,
        62311,72304,92871,87052,71300,
        80379,94063,37074,16653,30502,
        23172,50643,79462,41340,75098,
        16720,25250,34865,18459,44178,
        17912,44098,36895,31305,8510,
        80010,44693,27445,31841,18474,
        86190,94511,83414,44937,32525,
        6182,7534,56344,45044,92036,
        38565,94984,25863,13950,86503,
        1735,5848,65190,11271,76655,
        57302,33257,59388,59532,71410,
        62939,26551,21260,73451,94200,
        97731,58685,37115,53353,81350]

-- *Main> coldfiltered nums
-- ([81908,42956,51050,88276,46830,28368,66370,73908,32368,5100,53050,6304,54400,94638,47074,21280,49562,86028,18542,72304,87052,71300,37074,30502,23172,79462,41340,75098,16720,25250,44178,17912,44098,8510,80010,18474,86190,83414,6182,7534,56344,45044,92036,94984,13950,5848,65190,57302,59388,59532,71410,21260,94200,81350],[50965,95285,51050,46830,66370,30545,5100,81055,53050,54400,21280,23525,71300,41340,16720,25250,34865,36895,31305,8510,80010,27445,86190,32525,38565,13950,1735,65190,76655,71410,21260,94200,58685,37115,81350],[51050,46830,66370,5100,53050,54400,21280,71300,41340,16720,25250,8510,80010,86190,13950,65190,71410,21260,94200,81350],[46830,5100,41340,80010,86190,13950,65190,94200])

{-- A little background.

Div-by-whatever ... who cares?

Actually, fast factoring is a 'thing.' Encryption these days work with a
private-publicly-pair key set that make a number that's somewhat prime-like
(but not prime). That number is used to encrypt information, so, if that number
can be factored quickly, the public-(hidden)private key-pair can be known, and
then the information decrypted. Quickly.

When encrypting you want a hard-to-factor number, and, when trying to decrypt
information, you want to factor the key, fast.
--}