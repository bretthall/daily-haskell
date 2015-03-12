{--
Solution to @1HaskellADay http://lpaste.net/5006337360527360000 by @bretthall

Sorry for the one-day delay. Work, life, worklife got in the way.

Won't happen again.

(@geophf just admits he has no life.)

ANYWAY! *AHEM*

Chapter XII

The Thousand-and-Thirteenth Night, in Which Scheherazade Relates the Story
of Al-Khizr

"Tonight," said Scheherazade, "I will give you some puzzles of a completely
different nature than any I've given you so far."

Scheherazade then related the following story and puzzles.

117)) The First Test

A certain prince Al-Khizr was in love with the sultan's daughter and asked for
her hand in marriage.

"My daughter is very choosy," said the sultan, "and wants to marry only someone
who shows extraordinary intelligence. So if you want to marry her, you must
first pass eight tests."

"What are the tests?" asked the suitor.

"Well, for the first test, you have to write down a number that will be sent to
the princess. She will then send back a number to you. If she sends back the 
very same number that you have sent her, then she will allow you to take the
second test. But if her number is different from yours, then you are out."

"Now, how can I possibly know what number to write?" asked the suitor. "How can
I guess what number the princess has in mind?"

"Oh, she doesn't have a number in mind," said the sultan. "The number she sends
back is DEPENDENT on the number you send. The number you send /completely
determines/ what number she will send back. And if you send the right number,
then she will send back the same number."

"Then how can I guess the right number?" asked the suitor.

"It's not a matter of /guessing/," said the sultan. "You must /deduce/ the
correct number from the rules I am about to give you. For any number x and y,
by xy I mean not x times y but x followed by y, both numbers, of course, 
written in base ten Arabic notation. For example, if x is 5079 and y is 863,
then by xy I mean 5079863. Now here are the rules:

Rule 1: For any number x, if you write her 1x2, then she will send you back
        the number x. For example, if you write 13542, she will write back 354.

Rule 2: For any number x, the repeat of x means xx. For example, the repeat
        of 692 is 692692.  And now, the second rule is that if x brings back y,
        then 3x brings back the repeat of y.  For example, since 15432 brings
        back 543, then 315432 brings back 543543. From which it further follows
        that if you send her 3315432, you will get back 543543543543 (since
        315432 brings back 543543).

Rule 3: The reverse of a number means the number written backwards. For example,
        the reverse of 62985 is 58926. The third rule is that if x brings back
        y, then 4x brings back the reverse of y. For example, since 1729962
        brings back 7296, then 4172962 brings back 6927. Thus, if you send her
        the number 4172962, you will get back 6927. Or, combining rules 1, 2
        and 3, since 316982 brings back 698698 (by rules 1 and 2), then
        4316982 brings back 896896

Rule 4 (The Erasure Rule): If x brings back y, and if y contains at least two
        digits, then 5x brings back the result of erasing the first digit of y.
        For example, since 13472 brings back 347, then 513472 brings back 47.

Rule 5 (The Addition Rule): If x brings back y, then 6x brings back 1y and 7x
        brings back 2y. For example, since 15832 brings back 583, then 61532
        brings back 1583, and 715832 brings back 2583.

"Those are the rules," said the sultan, "and from them can be deduced a number
x that will bring back the very number . There are actually an infinite number
of solutionss, but any single one will suffice for passing the first test."

"Are there any /meanings/ to these numbers?" asked the suitor.

"Ah, that is the princess' secret, but fortunately you don't have to know the
meaning in order to pass the first test."

WHAT IS THE NUMBER X SUCH THAT THE PRINCESS SENDS BACK THAT SELF-SAME NUMBER?

--}
module Main where

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Control.Monad
import Control.Applicative
import Data.Maybe
import Data.List (find)
import Debug.Trace

numDigits :: Integral a => a -> a -> Int
numDigits b n = num n 0
    where
      num i d | i == 0 = d
              | otherwise = num (i `div` b) (d + 1)

type Digits = V.Vector Int

digits :: (Integral a) => a -> a -> Digits
digits b n = V.create $ do
                  let numDs = numDigits b n
                  v <- MV.new numDs
                  fill (numDs - 1) n v
                  return v
    where
      fill i n v = do
        let (rest, digit) = quotRem n b
        MV.write v i $ fromIntegral digit
        when (i > 0) $ fill (i - 1) rest v

unDigits :: (Integral a) => a -> Digits -> a
unDigits b v = V.foldl' (\a v -> a*b + fromIntegral v) 0 v

{--
Rules are mutually exclusive: given any number only one rule can apply to it (further rules can apply to the 
reults of applying the rule though).

A valid number is *completely* determined by the rules, and since the only rule that doesn't have a 
"if x brings back y" clause is rule 1 then we must always get to rule 1 via some chain of other rules.
If we don't get to rule 1 then the number is not "completely determined". 
--}

applyRules :: Digits -> Maybe Digits
applyRules ds | (not . V.null) ds = msum $ rules <*> pure ds
              | otherwise = Nothing

rules :: [Digits -> Maybe Digits]
rules = [rule1, rule2, rule3, rule4, rule5]

rule1 :: Digits -> Maybe Digits
rule1 ds | (V.head ds == 1) && (V.last ds == 2) = Just $ (V.tail . V.init) ds
         | otherwise = Nothing

applyRulesToTail :: Digits -> Maybe Digits
applyRulesToTail = applyRules . V.tail

rule2 :: Digits -> Maybe Digits
rule2 ds | V.head ds == 3 = fmap (\ds' -> ds' V.++ ds') $ applyRulesToTail ds
         | otherwise = Nothing

rule3 :: Digits -> Maybe Digits
rule3 ds | V.head ds == 4 =  fmap V.reverse $ applyRulesToTail ds
         | otherwise = Nothing

rule4 :: Digits -> Maybe Digits
rule4 ds | V.head ds == 5 = erase $ applyRulesToTail ds
         | otherwise = Nothing
         where
           erase (Just ds') | V.length ds' > 1 = Just $ V.tail ds'
                            | otherwise = Nothing
           erase Nothing = Nothing

rule5 :: Digits -> Maybe Digits
rule5 ds | V.head ds == 6 = fmap (V.cons 1) $ applyRulesToTail ds
         | V.head ds == 7 = fmap (V.cons 2) $ applyRulesToTail ds
         | otherwise = Nothing

numberX :: Integer -> Maybe Integer
numberX = fmap (unDigits 10) . applyRules . digits 10 

{-- Testing:
*Main> numberX 13542
Just 354
*Main> numberX 3315432
Just 543543543543
*Main> numberX 4316982
Just 896896
*Main> numberX 513472
Just 47
*Main> numberX 615832
Just 1583
*Main> numberX 715832
Just 2583

=> Seems to be working
--}

isGoodNumber :: Integer -> Bool
isGoodNumber n = trace ("checking " ++ show n) $ maybe False (n ==) (numberX n)

--This takes a really long time (so long that I don't have an answer yet), there's probably a smarter
--way to do it but I don't have time to mess with it for now. I'll leave goodNumber running and update
--if I get an answer
goodNumber :: Integer
goodNumber = fromJust $ find isGoodNumber [1..]

main = print goodNumber