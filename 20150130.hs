{-- 
Solution to @1HaskellADay http://lpaste.net/119587

Today is a prime day to take a break from triagles. Posted on twitter:

Numbers (< 1000) such that concatenation of their prime factors is a prime number
http://wp.me/p28Rc6-5DB  via @BenVitale #math

--}

import Data.Numbers.Primes

--note: I'm also filtering out the prime numbers since they trivially satisfy the condition, e.g. 11's prime factors are [11]
-- and catting ["11"] gives 11 which, while it is prime, is sort of boring
cattedPrimeIsPrime :: [Int]
cattedPrimeIsPrime = filter (isPrime.catNums.primeFactors) $ filter (not.isPrime) [4..1000]
    where
      catNums = read.concatMap show

{--
*Main> cattedPrimeIsPrime
[6,12,18,21,22,28,33,39,46,51,52,54,58,63,66,70,82,84,93,98,111,115,117,133,141,142,148,154,159,162,165,166,171,172,175,177,182,187,198,201,205,207,210,219,220,226,232,235,237,245,246,247,249,253,255,261,262,264,266,267,268,274,279,282,291,292,294,297,301,310,319,327,338,350,355,358,376,384,385,387,388,391,392,399,406,408,411,423,426,427,430,432,434,435,436,440,442,459,468,472,475,476,478,489,494,498,501,502,504,505,511,516,525,531,534,535,538,543,549,552,562,565,568,573,574,583,584,586,589,595,598,608,615,620,622,628,630,632,639,657,664,679,684,686,687,694,696,697,705,721,728,741,742,752,753,756,759,763,766,771,772,775,778,781,783,786,790,793,798,799,801,804,813,816,819,820,833,835,837,856,860,871,872,875,884,885,888,889,895,901,904,916,921,924,930,934,938,939,943,946,949,963,968,985,993]

=> There's a lot more than I thought there'd be
--}