{--
Solution for @1HaskellADay http://lpaste.net/114436
 
From Mensa's Genius Quiz-a-Day, problem for November 18th

Another Victorian-type poem in which the correct letter from each clue gives
you a new word

	My first is in ocean but not in sea
	My second in milk but not in me
	My third in three but not in throw
	My fourth in vow but not in crow
	My fifth in eight but not in night
	My last in wrong and also right
	My whole is praise for thoughts or men
	Or women, too, or tongue or pen.

--}

import Data.List
import Data.Char
import Control.Applicative
import System.IO.Unsafe

poem :: String
poem = "My first is in ocean but not in sea\n\
       \My second in milk but not in me\n\
       \My third in three but not in throw\n\
       \My fourth in vow but not in crow\n\
       \My fifth in eight but not in night\n\
       \My last in wrong and also right\n\
       \My whole is praise for thoughts or men\n\
       \Or women, too, or tongue or pen."

data Clue = Letter Int String | LastLetter String deriving (Show, Eq, Ord)

clues :: [Clue]
clues = [Letter 1 ("ocean" \\ "sea"),
         Letter 2 ("milk" \\ "me"),
         Letter 3 ("three" \\ "throw"),
         Letter 4 ("vow" \\ "crow"),
         Letter 5 ("eight" \\ "night"),
         LastLetter ("wrong" `intersect` "right")]

getLetters :: Clue -> String
getLetters (Letter _ ls) = ls
getLetters (LastLetter ls) = ls

possibleWords :: [String]
possibleWords = nub $ sort $ foldr possibles [] $ map getLetters $ sort clues
    where
      possibles p [] = map (:[]) p
      possibles p ps = (:) <$> p <*> ps

allowedWords :: [String]
allowedWords = unsafePerformIO $ do
                 text <- readFile "5desk.txt"
                 return $ map (map toLower) $ filter ((== 6).length) $ lines text

da6ltrWord :: [String]
da6ltrWord = filter (`elem` allowedWords) possibleWords

-- *Main Data.List Control.Applicative> da6ltrWord
-- ["clever"]

-- And clever satisfies the final clue as it "praises" all that stuff

--It would be interesting to write a Parsec parser that could parse the poem to build the clues,
--but I'm out of time for today.