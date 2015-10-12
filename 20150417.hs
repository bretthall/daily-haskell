import Control.Arrow
import Control.Monad
import Data.List

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B

{--

Okay, THAT problem was solved, but a couple of things:

One: I hate repeating values.
Two: I lost all my line-breaks, and if writing is meant to be read, then ...

So, one, instead of (a, b) = (take 4999 words, drop 4999 words) use the value
'words' only once (arrows, perhaps?) and have that value (pair) used once to
write out the two files (sequencing of Kleisli arrows?)

Yeah, look at (***) and (&&&) and sequence and is there a function that converts
a tuple to a list for sequencing?

And, two, break the original file into the first 5,000 words and 'morethen'
(seriously: 'more then.' It's like, I'm stuck on this inanity), but this time,
ensure that line breaks are preserved.

And, three (of the two gripes I had above), my processor doesn't read utf8,
can you believe that String is still caught up in the 3rd century B.C. or 
something like that? Use Data.Text instead of String so you can proceess
weirdo characters such as whatever is in the text files I'm processing, please.
 --}

type Line = T.Text
type Lines = [Line]
type Word = T.Text
type Words = [Word]
 
first5000 :: Lines -> (Lines, Lines)
first5000 ls = first5000' 0 [] (map T.words ls)
    where
      first5000' :: Int -> Lines -> [Words] -> (Lines, Lines)
      first5000' _ _ [] = (ls, [])
      first5000' n fs (c:cs) | n' < 5000 = first5000' n' (fs ++ [T.unwords c]) cs
                             | otherwise = (fs ++ [T.unwords $ take make5000 c], (T.unwords $ take leftOver c):(map T.unwords cs))
          where
            numWords = length c
            n' = n + numWords
            leftOver = n' - 5000
            make5000 = numWords - leftOver

youWantNewLinesWidDat :: FilePath -> IO ()
youWantNewLinesWidDat opusScriptus = do
  bs <- B.readFile opusScriptus
  let (a, b) = first5000 $ T.lines $ TE.decodeUtf8 bs
  B.writeFile "first.txt" $ TE.encodeUtf8 $ T.unlines a
  B.writeFile "morethen.txt" $ TE.encodeUtf8 $ T.unlines b
