{--
Solution to @1HaskellADay http://lpaste.net/120087

So, today is FRIDAY! YAY! (in case you didn't know)

For today's Haskell problem, please count the number of times
'Friday' is sung in Rebecca Black's eponymous song. Lyrics here:

http://lpaste.net/120086

--}

import Data.Text as T

itsFriday :: FilePath -> IO Int
itsFriday song = readFile song >>= return . countFridays

countFridays :: String -> Int
countFridays = (\n -> n - 1) . Prelude.length . (splitOn $ T.pack "friday") . T.toLower . T.pack

{--
*Main> itsFriday "20150206-lyrics.txt"
27
--}