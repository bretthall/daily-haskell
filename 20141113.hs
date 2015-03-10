{--

Solution to @1HaskellADay http://lpaste.net/114233

We're all truth-seekers here, right?

We're all refined people of exactitude, so, like, in cnn.com, when a scientist
reports that his interest is 'peaked' we do not blanch in shame, right? Because
we're simply doing this, whatever all this is, for the betterment of self,
knowledge and seeking out the Truth.

Constructively, of course.

It has nothing to do with making the grade, right?

Today's problem comes from American Mathematics Competition, by way of MAA
(the Mathematical Association of America):

"Isabella must take four 100-poit tests in her math class.

[Why 'must' she take these tests? Because she's a Truth-seeker, as we all are]

Her goal is to achieve an average grade of at least 95 on the tests. Her first
two tests scores were 97 and 91. After seeing her score on the third test, she
realized that she could still reach her goal. What is the lowest possible score
she could have made on the third test?"

Usually with these problems, MAA posts the percent of students who responded
with the correct answer (e.g.: "56% of students who responded ...")

But there was no such indication of success on this particular problem, perhaps
because all the math students got this one correct, given that they all have
been through this exercise in life? --}

--There's only 10,000 possibilities so give brute force a try

import Data.List (minimumBy)

possibleScores :: [[Int]]
possibleScores = [[97, 91, x, y] | x <- [0..100], y <- [0..100]]

avg :: [Int] -> Rational
avg = (/ 4).fromIntegral.sum

okScores :: [[Int]] -> [[Int]]
okScores = filter ((==95).round.avg)

lowestOnThird :: [[Int]] -> [Int]
lowestOnThird = minimumBy (\s1 s2 -> compare (s1 !! 2) (s2 !! 2))

isabellaMakesTheGrade :: Int
isabellaMakesTheGrade = ((!! 2).lowestOnThird.okScores) possibleScores

-- *Main> isabellaMakesTheGrade
-- 91

--Of course we could skip all that and just do the math:
-- (97 + 91 + x + y)/4 = 95 => x = 95*4 - 97 - 91 - y
--We'll get the minimum for x when we set y to the max, which is 100 in this case. That should be obvious but doing a check:
-- *Main> lowestOnThird $ okScores $ possibleScores
-- [97,91,91,100]

maxOnFourth :: Int
maxOnFourth = 100

becauseMath :: a -> a
becauseMath = id
              
isabellaMakesTheGrade' :: Int
isabellaMakesTheGrade' = becauseMath $ 95*4 - 97 - 91 - maxOnFourth 

-- *Main> isabellaMakesTheGrade'
-- 92
-- OOPS, It isn't specified but we need to know how the teacher deals with fractional averages; above I 
-- assumed that the value would be rounded, but this means that anything greater than 94.5 works and a score of 91
-- on the third test and 100 on the last gives us an average of 94.75. If instead we must have *at least* 95 then we
-- should floor in okScores instead of rounding:

okScores'' :: [[Int]] -> [[Int]]
okScores'' = filter ((==95).floor.avg)

isabellaMakesTheGrade'' :: Int
isabellaMakesTheGrade'' = ((!! 2).lowestOnThird.okScores'') possibleScores

-- *Main> isabellaMakesTheGrade''
-- 92

-- If we do have a hard floor of 95 on the average then we can avoid all the division and rounding:

okScores''' :: [[Int]] -> [[Int]]
okScores''' = filter ((==95*4).sum)

isabellaMakesTheGrade''' :: Int
isabellaMakesTheGrade''' = ((!! 2).lowestOnThird.okScores''') possibleScores

-- *Main> isabellaMakesTheGrade'''
-- 92
