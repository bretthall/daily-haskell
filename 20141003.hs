import Data.Set (Set)
import qualified Data.Set as Set 
import Control.Monad (liftM)
import qualified Data.ByteString.Char8 as B
import Data.Int
{--

Word ladders (originally called 'Doublets') by Lewis Carroll

So, geophf here, I wuz gonna go all 'prove the first n digits of pi are
a prime number' where n was leik, idk, 50 or so, because of a very interesting
article I read from aperiodical about how proving the primality of 83 using
rings (around Z, the complex number plane) allowed for the proof of any prime 
in linear time. But then I wuz leik, 'Nah!' I sed to myself. 'Self!' I sez,
'cause that's what I call myself when I'm talking to myself, 'Self!' I sez,
'I just did some number problems and some Euler problems and some logic
problems and the Missus is gettin' right ornery having no puzzles to solve
of the word-like variety!' So I thinks to myself, 'Well,' I thinks, 'I could
do the:'

             SEND
           + MORE
           ------
            MONEY

cryptarithmetic problem, but then, that still involves math and numbers, and
I don't wanna get me whacked upside the head by the good Missus demanding
a word problem and then finding out it involves numbers again, and why do I
have to be all like that, eh? tricking her like that? WHACK.

So, not a good option.

So I thinks to myself, 'Well,' and I thinks and I thinks, then I'm like:

BINGO!

(no, not BINGO, because that's really a boring game, computationally speaking)

BINGO! I sez, and I sez "WORD LADDERS!"

Now, there's me a piece of pie I can dig into for breakfast (of champions)!

So, like, Lewis Carroll has a whole book of these things (it's on the google
books for free) AND the Don himself, Donald Knuth, studied five-to-five word
ladders because he found that problem space 'not uninteresting.'

So, put that in your 'ceci n'est pas un pipe' and smoke it, eh?

Okay, word ladders.

Given an input word and an output word and an exact number of steps
inbetween source and target words, where at each step only one letter is
altered from the previous word to form the current word, solve the word 
ladder. There MAY be the case where hints are provided along the way, but 
let's just free-form over the /usr/share/dict/words (or whichever dictionary
you choose to use) for the time being in the allotted number of steps first, 
so, for example, from _Doublets, a Word Puzzle_ by Lewis Carroll (priced at 
'two shillings'), published 1879, the HEAD->TAIL puzzle goes thus:

HEAD
____
____
____
____
TAIL

and if we had defined the functions: --}

-- the set of words of length wordLength in the English language
dict :: Int -> IO ([B.ByteString])
dict wordLength = B.readFile "5desk.txt"  
                  >>= (\t -> return $ Prelude.filter (\w -> (B.length w) == wordLength) $ B.lines t)
-- get 5desk.txt from https://github.com/kevina/wordlist/tree/master/alt12dicts

fixedWords :: [(Int, Char)] -> [B.ByteString] -> [B.ByteString]
fixedWords ps words = foldl f words ps
    where 
      f ws (p, l) = filter (\w -> (B.index w p) == l) ws

fixedLetters freePos word = [(i, B.index word i) | i <- [0..((B.length word) - 1)], i /= freePos]

ladderStep :: [B.ByteString] -> Int -> B.ByteString -> [[B.ByteString]]
ladderStep words stepNum current | (stepNum < 0) = [[]]
                                 | otherwise = map (current:) $ concatMap step [0..lastPos]
    where
      lastPos = (B.length current) - 1
      step pos = concatMap (ladderStep words (stepNum - 1)) $ fixedWords (fixedLetters pos current) words

wordLadderBS :: B.ByteString -> B.ByteString -> Int -> [B.ByteString] -> [[B.ByteString]]
wordLadderBS source target interimSteps words = filter hitTarget $ ladderStep words interimSteps source
    where 
      hitTarget as = (as !! targetIndex) == target
      targetIndex = interimSteps - 1


-- NOt geting the right answer yet (plus it takes a really long time to get the wrong answer)
wordLadder :: String -> String -> Int -> [B.ByteString] -> [[B.ByteString]]
wordLadder source target interimSteps words = wordLadderBS (B.pack source) (B.pack target) interimSteps words

{--

The it would have returned as one of its possible solutions:

*Main> dict 4 >>= wordLadder "head" "tail" 4 ~>
IO [["heal", "teal", "tell", "tall"], ...]

Solve the following word ladders:

 --}

data Puzzle = Puz { source :: String, target :: String, interim :: Int }
   deriving Show

puzzle1, puzzle2, puzzle3, puzzle4, puzzle5, puzzle6, puzzle7 :: Puzzle

puzzle1 = Puz "pig" "sty" 4
puzzle2 = Puz "oat" "rye" 3
puzzle3 = Puz "four" "five" 6 -- the missus: "HA! I KNEW IT! NUMBERS! WHACK!"
puzzle4 = Puz "poor" "rich" 5
puzzle5 = Puz "mine" "coal" 5
puzzle7 = Puz "raven" "miser" 3
puzzle6 = Puz "flour" "bread" 5

solvePuzzel (Puz source target steps) = dict (length source) >>= (\ws -> return $ wordLadder source target steps ws)

--main = do
  