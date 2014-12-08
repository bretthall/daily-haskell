{--

Solution to @1HaskellADay http://lpaste.net/114437

From Mensa's Genuis Quiz-a-Day book, problem for November 22nd

Still wandering in Liars and Truthtellers Town, you run into two women, but
cannot tell if either tells the truth. You ask the first one, "Is either of you
a Truthteller?" After she answers, you know the truth. What did she say?

--}

import Data.List
import Data.Maybe
import Control.Arrow
import Control.Parallel.Strategies

data Woman = Liar | TruthTeller deriving (Eq, Ord, Show, Enum)
instance NFData Woman

possibilities :: [(Woman, Woman)]
possibilities = [(w1, w2) | w1 <- [Liar ..], w2 <- [Liar ..]]

data Answer = Yes | No deriving (Eq, Ord, Show, Enum)
instance NFData Answer

compatible :: Answer -> (Woman, Woman) -> Bool
compatible Yes (w1, w2) = w1 == TruthTeller || w2 == TruthTeller
compatible No (w1, w2) = w1 == Liar && w2 == TruthTeller

-- WARNING: gratuitous parallelism ahead (see http://lpaste.net/115749 for non-parallel version)
answers :: [(Answer, [(Woman, Woman)])]
answers = map (\a -> (a, filter (compatible a) possibilities)) [Yes ..] `using` parList rdeepseq
-- SPARKS: 2 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 2 fizzled)
-- => This is too trivial for the runtime to even get the parallel calculations set up before the main thread plows right 
--    though to the answer

-- Find an answer that has only one possiblity associated with it
herAnswer :: Maybe (Answer, (Woman, Woman))
herAnswer = listToMaybe $ map (second head) $ filter ((== 1).length.snd) answers

-- *Main> herAnswer
-- Just (No,(Liar,Liar))

main = print herAnswer

-- hint: you've been to this town before ... http://lpaste.net/112405

{-- hint 2:
 
... but that may be going overboard as that solution entailed
meta-logic (reasoning on the statements of statements of truth), this
one here seems to involve a simple coercion. That is to say, the
woman's response can possibly coerce the solution to be unique.

Which answer makes the wanderer's certainty justified? And, given that
answer, are the women both liars, both truthtellers or one-and-one?

--}
