{--

Solution to @1HaskellADay http://lpaste.net/116578

Counting Point Mutations solved by 7914
July 1, 2012, 8 p.m. by Rosalind Team Topics: Alignment

Evolution as a Sequence of Mistakes

Problem

Given two strings s and t of equal length, the Hamming distance between 
s and t, denoted dH(s,t), is the number of corresponding symbols that differ 
in s and t.

Given: Two DNA strings s and t of equal length (not exceeding 1 kbp).

Return: The Hamming distance dH(s,t).

Sample Dataset GAGCCTACTAACGGGAT CATCGTAATGACGGCCT

Sample Output
7

--}

import Data.ByteString.Char8 as B hiding (length, filter)
import Control.Monad (liftM)

dH :: String -> String -> Int
dH as bs = length $ filter id $ Prelude.zipWith (/=) as bs

-- Use ByteString as well for kicks
dHB :: B.ByteString -> B.ByteString -> Int
dHB as bs = length $ filter id $ B.zipWith (/=) as bs

sample1 = "GAGCCTACTAACGGGAT"
sample2 = "CATCGTAATGACGGCCT"

-- *Main> dH sample1 sample2
-- 7
-- => Seems to be working

-- *Main> dHB (B.pack sample1) (B.pack sample2)
-- 7
-- => Bytestrings working too

hammDataFile :: FilePath
hammDataFile = "20141216.txt" -- http://lpaste.net/116583

hammData :: IO [String]
hammData = liftM Prelude.lines (Prelude.readFile hammDataFile)

-- *Main> h <- hammData
-- *Main> dH (h !! 0) (h !! 1)
-- 467

hammDataB :: IO [ByteString]
hammDataB = liftM B.lines (B.readFile hammDataFile)

-- *Main> hb <- hammDataB
-- *Main> dHB (hb !! 0) (hb !! 1)
-- 467
