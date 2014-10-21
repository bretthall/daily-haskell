{--

From the Mensa Genius Quiz-a-Day book; October 17th's problem.

Write out this problem, and solve it, functionally:

"What is a four digit number, no zeros, in which the first number is five
times the last, the second is four more than the first and three times the
third, and the third is two more than the last and two less than the first?"

Easy enough to solve in your head, now, ... ;) put your head into Haskell code

--}

import Data.List (unfoldr)
import Control.Applicative ((<*>), pure)

type Number = (Int, Int, Int, Int)

fourDigitNumber :: [Number]

-- the returned value is the list of all possible solutions of the
-- first, second, third, and fourth digits

fourDigitNumber = filter applyRules numbers

-- yeah, anamorphism for once
numbers :: [Number]
numbers = unfoldr nextNum (0, 1, 1, 1)

type Rule = Number -> Bool

rule1 :: Rule
rule1 (x1, x2, x3, x4) = x1 == (5*x4)

rule2 :: Rule
rule2 (x1, x2, x3, x4) = x2 == (x1 + 4)

rule3 :: Rule
rule3 (x1, x2, x3, x4) = x2 == (3*x3)

rule4 :: Rule
rule4 (x1, x2, x3, x4) = x3 == (x4 + 2)

rule5 :: Rule
rule5 (x1, x2, x3, x4) = x3 == (x1 - 2)

rules :: [Rule]
rules = [rule1, rule2, rule3, rule4, rule5]

--applicative, goody
applyRules :: Number -> Bool
applyRules = and.(rules <*>).pure

increment :: Number -> Number
increment number = inc 0 number
    where
      inc n c | n < 4 = if x < 10 then set n x c else inc (n + 1) (set n 1 c)
              | otherwise = (0, 0, 0, 0)
              where
                x = 1 + get n c
                -- seems like there should be a better way to do this (probably should use UArray instead of a tuple)
                get n (x0, x1, x2, x3) | n == 0 = x0
                                       | n == 1 = x1
                                       | n == 2 = x2
                                       | n == 3 = x3
                                       | otherwise = undefined
                set n x (x0, x1, x2, x3) | n == 0 = (x, x1, x2, x3)
                                         | n == 1 = (x0, x, x2, x3)
                                         | n == 2 = (x0, x1, x, x3)
                                         | n == 3 = (x0, x1, x2, x)
                                         | otherwise = undefined

nextNum :: Number -> Maybe (Number, Number)
nextNum n | n == (0, 0, 0, 0) = Nothing
          | otherwise = Just (n', n')
    where
      n' = increment n

{--

Of course, you could create a typed numeral set where Zero is undefined
and the solution set only contains Successors of the Zero-type (which may
or may not be uninhabited) ... but do that only if you want to, eh?

e.g. What I'm NOT suggesting is this:

http://www.alfredodinapoli.com/posts/2014-10-13-fun-with-dependent-types-in-haskell.html

... not at all!

--}

{--

The title is an oblique reference to the cute, little song by Boy, and
four plus three is se7en, so there's some oblique-oblique relationship
there, if you're grasping at straws trying to understand the reference
and are looking to me for a reasonable explanation (never a good thing)

--}