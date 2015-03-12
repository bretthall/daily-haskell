{--

Solution to @1HaskellADay http://lpaste.net/114435

From Mensa's Genius Quiz-a-Day book, problem from November 17th

Can you go from GLOW to WORM in only seven steps, changing one letter at a
time and making a good English word at each step?

     G    L    O    W


     __   __   __   __


     __   __   __   __


     __   __   __   __


     __   __   __   __


     __   __   __   __


     __   __   __   __

     W    O    R    M

--}
module Main where

import Data.Char
import qualified Data.ByteString.Char8 as BS hiding (filter)
import Data.Map.Strict hiding (map, filter, null)
import Rules -- http://lpaste.net/114590
import Data.List (nub, sort)
import Control.Monad.Par

main = do
  ms <- createMaps
  let worms = glowWorm ms
  mapM_ print worms
  print $ length worms

--GHCI takes too long for this so:
--bhall $ghc -O2 20141204.hs
-- bhall $./20141204.exe
-- ["GLOW","BLOW","BLOB","BOOB","BOOM","DOOM","DORM","WORM"]
-- ["GLOW","BLOW","BLOB","BOOB","BOON","BORN","WORN","WORM"]
-- ["GLOW","BLOW","BLOT","BOOT","BOOM","DOOM","DORM","WORM"]
-- ["GLOW","BLOW","BLOT","BOOT","BOON","BORN","WORN","WORM"]
-- ["GLOW","BLOW","BLOT","BOOT","FOOT","FORT","FORM","WORM"]
-- ["GLOW","BLOW","BLOT","BOOT","FOOT","FORT","WORT","WORM"]
-- ["GLOW","BLOW","BLOT","BOOT","MOOT","MORT","WORT","WORM"]
-- ["GLOW","BLOW","BLOT","BOOT","SOOT","SORT","WORT","WORM"]
-- ["GLOW","BLOW","BLOT","BOOT","TOOT","TORT","WORT","WORM"]
-- ["GLOW","BLOW","BLOT","SLOT","SOOT","SORT","WORT","WORM"]
-- ["GLOW","BLOW","SLOW","SLOT","SOOT","SORT","WORT","WORM"]
-- ["GLOW","FLOW","SLOW","SLOT","SOOT","SORT","WORT","WORM"]
-- ["GLOW","GLOB","BLOB","BOOB","BOOM","DOOM","DORM","WORM"]
-- ["GLOW","GLOB","BLOB","BOOB","BOON","BORN","WORN","WORM"]
-- ["GLOW","GLOB","GLOP","GOOP","GOOD","WOOD","WORD","WORM"]
-- ["GLOW","GLOB","GLOP","GOOP","GORP","GORE","WORE","WORM"]
-- ["GLOW","GLOB","SLOB","SLOT","SOOT","SORT","WORT","WORM"]
-- ["GLOW","GLOM","GLOP","GOOP","GOOD","WOOD","WORD","WORM"]
-- ["GLOW","GLOM","GLOP","GOOP","GORP","GORE","WORE","WORM"]
-- ["GLOW","GLOP","CLOP","COOP","COOK","CORK","CORM","WORM"]
-- ["GLOW","GLOP","CLOP","COOP","COOK","CORK","WORK","WORM"]
-- ["GLOW","GLOP","CLOP","COOP","COON","CORN","CORM","WORM"]
-- ["GLOW","GLOP","CLOP","COOP","COON","CORN","WORN","WORM"]
-- ["GLOW","GLOP","GOOP","COOP","COOK","CORK","CORM","WORM"]
-- ["GLOW","GLOP","GOOP","COOP","COOK","CORK","WORK","WORM"]
-- ["GLOW","GLOP","GOOP","COOP","COON","CORN","CORM","WORM"]
-- ["GLOW","GLOP","GOOP","COOP","COON","CORN","WORN","WORM"]
-- ["GLOW","GLOP","GOOP","GOOD","FOOD","FORD","FORM","WORM"]
-- ["GLOW","GLOP","GOOP","GOOD","FOOD","FORD","WORD","WORM"]
-- ["GLOW","GLOP","GOOP","GOOD","FOOD","WOOD","WORD","WORM"]
-- ["GLOW","GLOP","GOOP","GOOD","GOAD","WOAD","WORD","WORM"]
-- ["GLOW","GLOP","GOOP","GOOD","GOLD","WOLD","WORD","WORM"]
-- ["GLOW","GLOP","GOOP","GOOD","HOOD","WOOD","WORD","WORM"]
-- ["GLOW","GLOP","GOOP","GOOD","MOOD","WOOD","WORD","WORM"]
-- ["GLOW","GLOP","GOOP","GOOD","ROOD","WOOD","WORD","WORM"]
-- ["GLOW","GLOP","GOOP","GOOD","WOOD","WOAD","WORD","WORM"]
-- ["GLOW","GLOP","GOOP","GOOD","WOOD","WOLD","WORD","WORM"]
-- ["GLOW","GLOP","GOOP","GOOD","WOOD","WORD","WORE","WORM"]
-- ["GLOW","GLOP","GOOP","GOOD","WOOD","WORD","WORK","WORM"]
-- ["GLOW","GLOP","GOOP","GOOD","WOOD","WORD","WORN","WORM"]
-- ["GLOW","GLOP","GOOP","GOOD","WOOD","WORD","WORT","WORM"]
-- ["GLOW","GLOP","GOOP","GOOF","GOOD","WOOD","WORD","WORM"]
-- ["GLOW","GLOP","GOOP","GOOF","WOOF","WOOD","WORD","WORM"]
-- ["GLOW","GLOP","GOOP","GOOK","COOK","CORK","CORM","WORM"]
-- ["GLOW","GLOP","GOOP","GOOK","COOK","CORK","WORK","WORM"]
-- ["GLOW","GLOP","GOOP","GOOK","GOOD","WOOD","WORD","WORM"]
-- ["GLOW","GLOP","GOOP","GOON","BOON","BORN","WORN","WORM"]
-- ["GLOW","GLOP","GOOP","GOON","COON","CORN","CORM","WORM"]
-- ["GLOW","GLOP","GOOP","GOON","COON","CORN","WORN","WORM"]
-- ["GLOW","GLOP","GOOP","GOON","GOOD","WOOD","WORD","WORM"]
-- ["GLOW","GLOP","GOOP","GOON","LOON","LORN","WORN","WORM"]
-- ["GLOW","GLOP","GOOP","GOON","MOON","MORN","WORN","WORM"]
-- ["GLOW","GLOP","GOOP","GORP","GORE","BORE","WORE","WORM"]
-- ["GLOW","GLOP","GOOP","GORP","GORE","CORE","CORM","WORM"]
-- ["GLOW","GLOP","GOOP","GORP","GORE","CORE","WORE","WORM"]
-- ["GLOW","GLOP","GOOP","GORP","GORE","DORE","DORM","WORM"]
-- ["GLOW","GLOP","GOOP","GORP","GORE","DORE","WORE","WORM"]
-- ["GLOW","GLOP","GOOP","GORP","GORE","FORE","FORM","WORM"]
-- ["GLOW","GLOP","GOOP","GORP","GORE","FORE","WORE","WORM"]
-- ["GLOW","GLOP","GOOP","GORP","GORE","LORE","WORE","WORM"]
-- ["GLOW","GLOP","GOOP","GORP","GORE","MORE","WORE","WORM"]
-- ["GLOW","GLOP","GOOP","GORP","GORE","PORE","WORE","WORM"]
-- ["GLOW","GLOP","GOOP","GORP","GORE","SORE","WORE","WORM"]
-- ["GLOW","GLOP","GOOP","GORP","GORE","TORE","WORE","WORM"]
-- ["GLOW","GLOP","GOOP","GORP","GORE","WORE","WORD","WORM"]
-- ["GLOW","GLOP","GOOP","GORP","GORE","WORE","WORK","WORM"]
-- ["GLOW","GLOP","GOOP","GORP","GORE","WORE","WORN","WORM"]
-- ["GLOW","GLOP","GOOP","GORP","GORE","WORE","WORT","WORM"]
-- ["GLOW","GLOP","GOOP","GORP","GORE","YORE","WORE","WORM"]
-- ["GLOW","GLOP","GOOP","GORP","GORY","DORY","DORM","WORM"]
-- ["GLOW","GLOP","GOOP","GORP","GORY","GORE","WORE","WORM"]
-- ["GLOW","GLOP","GOOP","HOOP","HOOD","WOOD","WORD","WORM"]
-- ["GLOW","GLOP","GOOP","LOOP","LOOM","DOOM","DORM","WORM"]
-- ["GLOW","GLOP","GOOP","LOOP","LOON","LORN","WORN","WORM"]
-- ["GLOW","GLOP","SLOP","SLOT","SOOT","SORT","WORT","WORM"]
-- ["GLOW","GROW","GROT","TROT","TOOT","TORT","WORT","WORM"]
-- ["GLOW","GROW","TROW","TROT","TOOT","TORT","WORT","WORM"]
-- ["GLOW","PLOW","PLOT","SLOT","SOOT","SORT","WORT","WORM"]
-- ["GLOW","PLOW","SLOW","SLOT","SOOT","SORT","WORT","WORM"]
-- ["GLOW","SLOW","SCOW","SCOT","SOOT","SORT","WORT","WORM"]
-- ["GLOW","SLOW","SHOW","SHOT","SOOT","SORT","WORT","WORM"]
-- ["GLOW","SLOW","SLOB","SLOT","SOOT","SORT","WORT","WORM"]
-- ["GLOW","SLOW","SLOE","SLOT","SOOT","SORT","WORT","WORM"]
-- ["GLOW","SLOW","SLOG","SLOT","SOOT","SORT","WORT","WORM"]
-- ["GLOW","SLOW","SLOP","SLOT","SOOT","SORT","WORT","WORM"]
-- ["GLOW","SLOW","SLOT","SCOT","SOOT","SORT","WORT","WORM"]
-- ["GLOW","SLOW","SLOT","SHOT","SOOT","SORT","WORT","WORM"]
-- ["GLOW","SLOW","SLOT","SNOT","SOOT","SORT","WORT","WORM"]
-- ["GLOW","SLOW","SLOT","SOOT","FOOT","FORT","FORM","WORM"]
-- ["GLOW","SLOW","SLOT","SOOT","FOOT","FORT","WORT","WORM"]
-- ["GLOW","SLOW","SLOT","SOOT","MOOT","MORT","WORT","WORM"]
-- ["GLOW","SLOW","SLOT","SOOT","SOFT","SORT","WORT","WORM"]
-- ["GLOW","SLOW","SLOT","SOOT","SORT","FORT","FORM","WORM"]
-- ["GLOW","SLOW","SLOT","SOOT","SORT","FORT","WORT","WORM"]
-- ["GLOW","SLOW","SLOT","SOOT","SORT","MORT","WORT","WORM"]
-- ["GLOW","SLOW","SLOT","SOOT","SORT","PORT","WORT","WORM"]
-- ["GLOW","SLOW","SLOT","SOOT","SORT","SORE","WORE","WORM"]
-- ["GLOW","SLOW","SLOT","SOOT","SORT","TORT","WORT","WORM"]
-- ["GLOW","SLOW","SLOT","SOOT","SORT","WORT","WORD","WORM"]
-- ["GLOW","SLOW","SLOT","SOOT","SORT","WORT","WORE","WORM"]
-- ["GLOW","SLOW","SLOT","SOOT","SORT","WORT","WORK","WORM"]
-- ["GLOW","SLOW","SLOT","SOOT","SORT","WORT","WORN","WORM"]
-- ["GLOW","SLOW","SLOT","SOOT","TOOT","TORT","WORT","WORM"]
-- ["GLOW","SLOW","SLOT","SPOT","SOOT","SORT","WORT","WORM"]
-- ["GLOW","SLOW","SNOW","SNOT","SOOT","SORT","WORT","WORM"]
-- => 105 Solutions!!

-- paralleized using Control.Monad.Par 
-- Speedup with -N4 over the non-parallel version is ~2.6
-- not bad for the minimal amount of effort put into it

start :: BS.ByteString
start = BS.pack "GLOW"

target :: BS.ByteString
target = BS.pack "WORM"

numSteps = 7

type WordSeq = [BS.ByteString]

glowWorm :: CharMaps -> [WordSeq]
glowWorm ms = nub $ sort $ runPar $ findWordSeqs ms target 7 start
-- Need the nub.sort above because we can get repeats of words like BOOT where multiple predessors 
-- can generate it by changing different letters (e.g. BLOT -> BOOT and BOLT -> BOOT)

readAllowedWords :: IO [BS.ByteString]
readAllowedWords = BS.readFile "5desk.txt" >>= return . map (BS.map toUpper) . filter ((==4) . BS.length) . BS.lines

type CharMap = Map Char [BS.ByteString]
type CharMaps = (CharMap, CharMap)

createMaps :: IO CharMaps
createMaps = do
  allowedWords <- readAllowedWords
  let onFstChar = fromAscListWith (flip (++)) [(BS.head w, [w]) | w <- allowedWords]
  let onSndChar = fromListWith (flip (++)) [(BS.index w 1, [w]) | w <- allowedWords]
  return (onFstChar, onSndChar)

lettersEqual :: BS.ByteString -> Int -> BS.ByteString -> Bool
lettersEqual w i = (== BS.index w i).(`BS.index` i)

findWords :: CharMaps -> BS.ByteString -> Int -> [BS.ByteString]
findWords (_, m2) w 0 = filter (satisfiesAll [lettersEqual w 3, lettersEqual w 2, not.lettersEqual w 0]) $ findWithDefault [] (BS.index w 1) m2
findWords (m1, _) w i = filter (satisfiesAll [lettersEqual w i3, lettersEqual w i2, not.lettersEqual w i]) $ findWithDefault [] (BS.index w 0) m1
    where
      i2 = if i == 1 then 2 else 1
      i3 = if i == 3 then 2 else 3
      
findAllSuccessors :: CharMaps -> BS.ByteString -> [BS.ByteString]
findAllSuccessors ms w = concatMap (findWords ms w) [0..3]


findWordSeqs :: CharMaps -> BS.ByteString -> Int -> BS.ByteString -> Par [WordSeq]
findWordSeqs _ target 0 start | start == target = return [[start]]
                              | otherwise = return []
findWordSeqs ms target steps start = possible >>= resultFrom
    where 
      possible = possible' >>= return . filter (not.null) . concat
      possible' = parMapM (findWordSeqs ms target (steps - 1)) $ findAllSuccessors ms start
      resultFrom [] = return []
      resultFrom ps = return $ map (start:) ps