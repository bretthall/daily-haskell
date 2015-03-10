{--
Solution for @1HaskellADay http://lpaste.net/121034
 
Saved by the tweets of @BenVitale of fun-with-numb3rs:

Benjamin Vitale @BenVitale  17h17 hours ago
there's an 8-digit square that can be transformed into another square if the 
2nd digit from the left is increased by 1 #math

I herein anoint this week as the fun-with-numb3rs week, becuase we're, like,
going to be having fun with numbers this week.

Like.

Okay, so I admit, to my shame, that I misread the above challenge. I read it
as '8-digit prime ... into another prime ...' which would be an entirely
different problem domain.

These are 'just' 8-digit squares so this is so-the-much-more-easierest now
('easierest' is now a word. Like 'ginormous.' (that's a word) (really, it is))

--}

import Data.Digits
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed ((!))
import qualified Data.Vector.Algorithms.Search as A
import Control.Monad.ST

hasEightDigits = (==8).length
--can't increment the 2nd digit if it is 9 since 10 is not a decimal digit
secondDigitGood = (< 9).(!! 1)

possible :: [[Int]]
possible = filter secondDigitGood $ takeWhile hasEightDigits $ dropWhile (not.hasEightDigits) $ map (digits 10.(\x -> x*x)) [1..]

possibleNums :: U.Vector Int
possibleNums = U.fromList $ map (unDigits 10) possible

find :: Int -> U.Vector Int -> Maybe Int
find n ns = runST $ do
              -- There is no canned binary serach for immutable vectors and I'm too lazy to write my own so we'll 
              -- pull some "unsafe" shenanigans here (really this should be perfectly safe since no one else is 
              -- using the possibleNums vector right now and we aren't going to modify it)
              ns' <- U.unsafeThaw ns
              i <- A.binarySearchL ns' n
              return $ if (i < U.length ns) && (ns ! i) == n
                       then Just i
                       else Nothing

transformas :: [(Int, Int)]
transformas = t possible
    where
      t [] = []
      t (n@(n1:n2:nr):ns) = let
          n' = unDigits 10 (n1:(n2 + 1):nr)
          in
            case find n' possibleNums of
              Just _ -> (unDigits 10 n, n'):t ns
              Nothing -> t ns
                                    
-- *Main> transformas
-- [(24502500,25502500)]
