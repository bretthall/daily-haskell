--- Solution (attempted) for @1HaskellADay http://lpaste.net/120388

-- For today, from @KenKenPuzzle we have this:

import Data.List
import Data.Char

scrambled :: String
scrambled = "Gurer ner gvzrf jura cneragubbq frrzf abguvat ohg srrqvat " 
            ++ "gur zbhgu gung ovgrf lbh. -crgre qr ievrf"

-- https://www.facebook.com/photo.php?fbid=872629029447151

-- Solve it.

unscrambled :: String -> String
unscrambled scrambledString = undefined

{-- Hints ... you lookin' at me for hints?

Well, there are four words of three (different) letters. You can scan your
dictionary finding three-letter words and seeing how applying them to this
problem (making sure they work throughout with the given substitutions) help
to unlock the unscrambling of this solution. --} 

scrambledWithoutSignature = takeWhile (\c -> c /= '.') scrambled

sortedWords = sortBy (\x y -> compare (length x) (length y)) $ words $ map toLower scrambledWithoutSignature

-- I think I see how to do it, but out of time for today