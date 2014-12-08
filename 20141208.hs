{--

Solution to @1HaskellADay http://lpaste.net/115961

Okay, I've just fallen, helplessly (hopelessly?) in love with a 
girl named Rosalind. She's smart, sweet, really funny, and into
bioinformatics, big-time.

I mean, like: big time! http://rosalind.info/about/

She does have a thing for Python, but we must all forgive each
other our foibles, mustn't we?

Anyway, dear, sweet, Rosalind, has me captured at one of her
many castles, wherein each night she proposes a new puzzle for
me to solve. Today, the first puzzle is as follows,

http://rosalind.info/problems/dna/

------

Counting DNA Nucleotides

Problem

A string is simply an ordered collection of symbols selected from some 
alphabet and formed into a word; the length of a string is the number of 
symbols that it contains.

An example of a length 21 DNA string (whose alphabet contains the symbols 
'A', 'C', 'G', and 'T') is "ATGCTTCAGAAAGGTCTTACG."

Given: A DNA string s of length at most 1000 nt.

Return: Four integers (separated by spaces) counting the respective number of 
times that the symbols 'A', 'C', 'G', and 'T' occur in s.

Sample Dataset
AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC

Sample Output
20 12 17 21

-----

Of course, with such as string, ANYONE could simply count that, so, instead,
she gave me a much larger string. It is located at http://lpaste.net/115962
as the file named 'rosalind_dna.txt.'

YOU can either copy-paste the string into your dna-counter or you can
save it off as a file and count nucleotides that way. --}

import Data.Vector.Unboxed as V
import Data.Vector.Generic.Mutable as MV

type As = Int
type Cs = Int
type Gs = Int
type Ts = Int

type Counts = V.Vector Int

aIndex = 0
cIndex = 1
gIndex = 2
tIndex = 3

dna :: String -> Counts
dna nucleotides = V.create $ do
                    counts <- MV.replicate 4 0
                    Prelude.mapM_ (count counts) nucleotides
                    return counts
    where
      count v c | c == 'A' = increment v aIndex
                | c == 'C' = increment v cIndex
                | c == 'G' = increment v gIndex
                | c == 'T' = increment v tIndex
                | otherwise = undefined
      increment v i = MV.unsafeRead v i >>= MV.unsafeWrite v i . (+ 1)

-- *Main> dna "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC"
-- fromList [20,12,17,21]

-- => Seems to be working

-- *Main> dna "CGCGCAGGTTGTGAGTTGTCGTAACTGGAGCAGAAGAACTCCCCAGACAAGAAAACCTTGACCATAGAACCACTCAGCTAAATCTCGTAAGTAAAATCACTGGTAGCTTGCCTATTAGTTTGCGTTTACTTTATAGATTACTAGGTCTCTACAATGGATACAGCTTGAAAATGTCTGATTGTACCTTGATCTGGAGGAAGTTTAGCCGGACCGGATGTAGTACAGGCAGCCACATGCAATGACTCCAGATACCACGTTTTGCCCGGTTCGCGTAAGCCAGAAATCGAGCACGCTGTAGGAGGTGGTTACCATTAGTACAGCGACAGGGGTGGTTGGCGCCGCACACCATCTGGAATCTAACCATCGTCGAGACGGTCATCACTGCGCAGGTCCCCAACAGCCTCGCTAGTGGTAGATGCTCGACATTAATAATAATCATCGAATTGTAGGGGACCATGTCAATCATCACGAAGAAAGCATATGTACTACACGCACTCCTCCATGTATCTGCGAGGACTCTCGGGCGAGGCCAACCTTGATTTTCGAATTACGAACCTGAATCCCGCGTTGCTGTTATTAGCTGCAGTTGATTTATATATTTCGGACGGAGTGGGGTATCATGCTCTATTAGACGCCATTGCACCTGAAAATTTGCTTTAGAGGGCAAGTGCTACGACGAGCCAACAGCATCCCTTATATATACATCTGTAACATAGAGCGGCTGTTGACGAACCGCAGGGCACCTCTCAGAGTTCGTATTTCAGCGGGATGTCGATCGGGTCAAGTCTCGAAAACCACCAATGGTGGGCTACGGTGCGCAAAATAGTGAGTCACGACCGTGCCTCCCACTTAGCGTCCCGATTACCAATGTTGGATATTTCAGAAGCGGCCGTTACATTAGGCAGTGAACATATGTAAGTACATATTTGAACACCAAATA"
-- fromList [257,222,226,235]

-- Lots of optimization opportunities here that I don't have time to pursue: use ByteString instead of String, 
-- combine 8 Char's into a Word64 and process 8 characters at a time, parallelize it...
-- But getting the above result takes a second at most in ghci so it's good enough for now