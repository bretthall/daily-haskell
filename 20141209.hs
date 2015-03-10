{--

Solution to @1HakellADay http://lpaste.net/116068

I think, but I am not certain (perhaps I AM certain, but perhaps it is
that I am uncertain if I am certain), that this week belongs me to my
Rosalind, dear Rosalind.

So, in EE class remember (micro-)coding hex-to-decimal and decimal-to-hex
circuits? So you could add, subtract, multiply and (never, ever) divide
in decimal-counting, and then up-covert the result into hex to 'return'
the result (load the result, more-belike) in the accumulator?

Remember doing that? Or am I way too old for the above statement to have
any relevance to the au courant crowd?

Seriously, you don't remember this? What a ... GAH! ... WASTE! Why not
just do your operations in hex and be done with it, but no! Accounting
and payroll software had to be coded, IN MACHINE LANGUAGE, with
decimal arithmetic, so there we were printing hex-to-dec and dec-to-hex
circuits.

Well, I'm going to make all that up to you now, those of you who did,
and those of you who did not have to suffer through that nonsense.

Here's a converter that REALLY matters: DNA-to-RNA, or Rosalie's ...

[oops! Slipped into my 'My Sister Rosalie'-Twilight mode] ...

or RosaLIND's problem 2:

Transcribing DNA into RNA

Problem

An RNA string is a string formed from the alphabet containing 'A', 'C', 'G',
and 'U'.

Given a DNA string t corresponding to a coding strand, its transcribed RNA
string u is formed by replacing all occurrences of 'T' in t with 'U' in u.

Given: A DNA string t having length at most 1000 nt.

Return: The transcribed RNA string of t.

Sample Dataset
GATGGAACTTGACTACGTAAATT

Sample Output
GAUGGAACUUGACUACGUAAAUU

--}

module Main where

import Data.ByteString as B
import Data.ByteString.Lazy as BL
import Data.Char
import Data.Word
import System.Environment
import Data.Maybe
import Control.Parallel.Strategies

dna2rna :: Char -> Char
dna2rna x = if x == 'T' then 'U' else x

dna2rna8 :: Word8 -> Word8
dna2rna8 x = if x == t8 then u8 else x
    where 
      t8 = fromIntegral $ ord 'T'
      u8 = fromIntegral $ ord 'U'

rna :: String -> String
rna = Prelude.map dna2rna

rnaBS :: B.ByteString -> B.ByteString
rnaBS = B.map dna2rna8

{-- Okay, the above is simple enough. Again, you can do it by hand. But 
what if you have to do it on something a little bigger?

Transcribe the DNA strand found at http://lpaste.net/116069 called 
rosalind_rnd.txt if you put that on your local file system. 

Bah, http://lpaste.net/116069 is too small to do any perfomance testing, instead use 
http://lpaste.net/116095 to generate a 100MB dna file
--}

type Method = FilePath -> FilePath -> IO ()

dna2RnaString :: Method
dna2RnaString inFile outFile = Prelude.readFile inFile >>= Prelude.writeFile outFile . rna

-- ~2x speedup just by switching to bytestrings
dna2RnaByteString :: Method
dna2RnaByteString inFile outFile = B.readFile inFile >>= B.writeFile outFile . rnaBS

-- another ~2x speedup by doing a simple parList style parallelization using 4 threads (not the greatest
-- thread scaling, but its all I have time for right now)
dna2RnaByteStringParallel :: Method
dna2RnaByteStringParallel inFile outFile = do
  dna <- BL.readFile inFile
  let dnaChunks = BL.toChunks dna
  let rnaChunks = Prelude.map rnaBS dnaChunks `using` parList rdeepseq
  BL.writeFile outFile $ BL.fromChunks rnaChunks

method :: String -> Method
method name = fromJust $ Prelude.lookup name methods
    where
      methods = [("string", dna2RnaString), 
                 ("bs", dna2RnaByteString),
                 ("bsp", dna2RnaByteStringParallel)]

main = do
  [name, inFile, outFile] <- getArgs
  method name inFile outFile