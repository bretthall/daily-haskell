{--
Solution to @1HaskellADay http://lpaste.net/120087

So, today is FRIDAY! YAY! (in case you didn't know)

For today's Haskell problem, please count the number of times
'Friday' is sung in Rebecca Black's eponymous song. Lyrics here:

http://lpaste.net/120086

--}

--Should probably figure out this new-fangled Data.Text business, now seems like as good a time as any,
--plus there's no splitOn for regular strings
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B

itsFriday :: FilePath -> IO Int
itsFriday song = B.readFile song >>= return . countFridays . TE.decodeUtf8

countFridays :: T.Text -> Int
countFridays = (\n -> n - 1) . length . (T.splitOn $ T.pack "friday") . T.toLower

{--
After much time figuring out how to fix my "multiple bytestring's installed" woes we get:

*Main> itsFriday "20150206-lyrics.txt"
27
--}