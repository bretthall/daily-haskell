module Data.Trie where

import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map

{-- Let's take a look at the Trie data structure:

http://en.wikipedia.org/wiki/Trie

Why? The problem with Data.Set is that after a certain number of strings, the
Set starts to implode, giving bad results for membership. Specifically, when
I loaded /usr/share/dict/words and queried for words when I was doing puzzles
(word-puzzles, specifically and surprisingly. ... I know, right? SHOCKER!), I
was getting wrong answers: some words that were in the words file were not
in the Set constructed from it.

Why?

This is a well-understood problem: when you hash a value, you run the risk
for a large number of values, to have your hash key collide for two objects
that are not isomorphic. When that happens, on an insert, one of the two objects
is lost, being overwritten.

Bummer!

FURTHER!

Sets are great, but the word 'asps' is actually four words in the dictionary:
'a,' 'as,' 'asp,' and 'asps,' so you'll have three entries in your set.

Not so with tries: you'll have one entry:

(a) -> (as) -> (asp) -> (asps)

And so when you do a find word for any of those four words, you'll traverse
that on search branch, and return true on a positive match

AND

What about 'ask' 
               ___ (ask)
              /
(a) -> (as) -<
              \___ (asp) -> (asps)

So, still one main branch, and now all five words are stored.

SWEET!

The write up in wikipedia has a skeletal declaration for the Trie-type, and in
Haskell, no less, because they are smart people, obviously, but they don't
go much beyond that.

Let's! --}

data Trie a = Tri { value :: Maybe a, children :: Map Char (Trie a) }
   deriving (Eq, Ord, Show)

find :: String -> Trie a -> Maybe a
find [] t = value t
find (k:ks) t = Map.lookup k (children t) >>= find ks 

-- so we've found find. No problem, as it was already defined for us.

-- ... but insert wasn't. Today's Haskell problem: define insert.


empty :: Trie a
empty = Tri Nothing Map.empty

--Only seem to be able to get it to compile for Trie String and out of time for today
insert :: String -> Trie String -> Trie String
insert word trie = insert' word trie
  where
    insert' [] (Tri _ cs) = Tri (Just word) cs
    insert' (l:ls) (Tri val cs) = case Map.lookup l cs of
                                   Just t -> Tri val $ Map.insert l (insert' ls t) cs
                                   Nothing -> Tri val $ Map.insert l (finish ls) cs
    finish [] = Tri (Just word) Map.empty
    finish (l:ls) = Tri Nothing $ Map.singleton l $ finish ls
   
-- Are the following words in your trie?
-- about, get, name, then, thenty

{--
λ> let words = ["about", "get", "name", "then", "thenty"]
λ> let t = foldr insert empty words
λ> map (\w -> find w t) words
[Just "about",Just "get",Just "name",Just "then",Just "thenty"]
λ> find "somthingelse" t
Nothing
λ> find "anotherwordthatsnotinthere" t
Nothing
=> Seems to be working
--}

-- BONUS -------------------------------------------------------------------

{-- With the definition of insert, above, read in /usr/share/dict/words into
your trie and report out the following: For each letter of (your) alphabet,
1: how many branches does each letter have?
2: how many members (total) does each branch of each letter have?
--}
