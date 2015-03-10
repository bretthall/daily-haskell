import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Data.List
import Data.Maybe

{--

Solution to @1HaskellADay http://lpaste.net/120422

From @OnThisDayinMath

2015 The first of three Friday the 13ths this year. When will the next year be 
with three?  

http://pballew.blogspot.com/2015/02/on-this-day-in-math-february-13.html :

2015 The first of three Friday the 13ths this year. They occur in Feb, Mar, 
and Nov. The last year with three Friday the 13ths was 2012. The 2012 triplet 
were in a leap year, and each 13 weeks apart (Jan, Apr, and July). This can 
only happen in a leap year. There seems to be no written evidence of the 
superstition in English until 1869. Interestingly, the Spanish and Greek 
Cultures have a similar tradition about Tuesday the 13th. *Wik 

--}

type Year = Integer
type Month = Int

monthsWithFridayThe13th :: Year -> [Month]
monthsWithFridayThe13th y = [m | m <- [1..12], (dayOfWeek $ toWeekDate $ fromGregorian y m 13) == 5]
    where 
      dayOfWeek (_, _, d) = d

countFridayThe13ths :: Year -> Int
countFridayThe13ths y = length $ monthsWithFridayThe13th y

threeFriday13ths :: Year -> Year
threeFriday13ths lastYear = fst.fromJust $ find (\(_, n) -> n == 3) $ map (\y -> (y, countFridayThe13ths y)) [lastYear + 1..]

{-- Hints:

The imports are hints. Hint-hint!

Also, threeFriday13ths 2011 ~> 2012
      threeFriday13ths 2012 ~> 2015
      threeFriday13ths 2015 ~> ?

Doit toit! --}

{--
*Main Control.Applicative> threeFriday13ths 2011
2012
*Main Control.Applicative> threeFriday13ths 2012
2015
*Main Control.Applicative> threeFriday13ths 2015
2026
--}