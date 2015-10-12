{-# LANGUAGE OverloadedStrings #-}

import Control.Monad

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Attoparsec.ByteString.Char8 as A 
import qualified Data.Attoparsec.ByteString.Lazy as AL

{--

Okay, we have a set of tags called 'TMQER' and 'other junk.' What we are doing is
exploring dependencies. Tags (may) have parents and (may) have children.

'Child' tags are blocked if one of its parents is blocked.

We need to know the hot-spots. Which tags have the most descendents?
That is: if a tag is blocked, are many dependent-tags blocked
because of it?

So, we wish to convert the file of tag|parents|children to a form
where we can do this exploration.

AND we wish to get rid of the junk.

Eventually, we will create a forest-like data structure that when queried with 
a tag, gives all tags dependent on it, including children, grand-children, great-
grand-children, all the way down ('turtles').

The file of pipe-delimited dependencies is at http://lpaste.net/1590149909779054592

AND when the file is scanned in, process ONLY tags, that is, process
only strings of the form 'TMQER' ++ some other stuff. FURTHERMORE, tags
in the child and parent files may have a '[t]' appended. Discard the '[t]'
from the tag names. --}

data Tag = R String deriving (Eq, Ord, Show)

data TagNode = TagNode {name::Tag, parents::[Tag], children::[Tag]} deriving Show

type DAG = [TagNode]

elemsParser :: A.Parser [String] 
elemsParser = (A.many' $ A.satisfy $ A.notInClass ",|\n") `A.sepBy` (A.string ", ") 

lineParser :: A.Parser TagNode
lineParser = do
  name <- A.many' $ A.notChar '|'
  A.char '|'
  parents <- elemsParser
  A.char '|'
  children <- elemsParser
  return $ TagNode (R name) (map R parents) (map R children)

fileParser :: A.Parser DAG
fileParser = lineParser `A.sepBy` A.endOfLine

buildDAG :: B.ByteString -> IO (Maybe DAG)
buildDAG s = case AL.parse fileParser s of
               AL.Done "" r -> return $ Just r
               AL.Done s r -> do
                 putStrLn $ "Failed to parse all input at: " ++ (take 40 $ B.unpack s)
                 putStrLn $ "Got: " ++ (show r)
                 return Nothing
               AL.Fail s _ e -> do
                 putStrLn $ "Parse failed '" ++ e ++"' at: " ++ (take 40 $ B.unpack s)
                 return Nothing

parseDAG :: FilePath -> IO DAG
parseDAG file = do
  s <- B.readFile file
  dag <- buildDAG s
  case dag of
    Nothing -> return []
    Just d -> return d 

-- tagM discards LAW3[t], changes TMQER219[t] to m (R TMQER219), lifts TMQER9
tagM :: MonadPlus m => String -> m Tag
tagM val = if (take prefixLength val) == prefix
           then return $ R withoutTail
           else mzero
    where
      prefix = "TMQER"
      prefixLength = length prefix
      suffix = "[t]"
      suffixLength = length suffix
      withoutTail = if valSuffix == suffix
                    then valWithoutSuffix
                    else val
      (valWithoutSuffix, valSuffix) = splitAt splitPt val
      splitPt = (length val) - suffixLength

-- So our task today is just to parse in the file and eliminate 'junk' tags.

-- Questions: How many parents and children do the following tags have? 
-- What are they?

-- 1. TMQER2 (hint: according to above, your answer should be 0 for both)
-- 2. TMQER30-2
-- 3. TMQER71

-- Which tag has the most children?

-- Tomorrow we'll look at extended families of tags.