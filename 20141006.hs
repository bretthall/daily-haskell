module Main where

{--

Dear Dad,

[the young man writes]

I find myself overcome by the disease infecting many of my classmates, and that
is maltuition. Would you please be so kind as to send me your next three
paychecks? These frat parties aren't going to run themselves, you know.

signed, your ever devoted son, etc

Dear Son,

[The Dad writes right back]

[This was a time when people wrote letters, you see.]

[So the return letter got to the son, like, two weeks later.]

Once again, you have asked me to send more money, but I find that I wish
to know if this is fruitful for you to continue your education at this fine
instution, a term used also to describe prisons, my alma mater (the school,
not the prison).

So, I will send you this amount of MONEY, after you first solve this problem:

     SEND
   + MORE
   ------
    MONEY

How much did the father send his son, if each letter represents a digit?

Write a haskell problem to solve this cryptarithmetic problem --}

import Data.List (foldl')

calcNum exps mults vs = foldl' (\c (e, (m, x)) -> c + m*x*(10^e)) 0 $ zip exps $ zip mults vs
calcSend = calcNum [3,2,1,0,0,0,0,0] [1,1,1,1,0,0,0,0]
calcMore = calcNum [0,0,0,0,3,2,1,0] [0,1,0,0,1,1,1,0]
calcMoney = calcNum [0,1,2,0,4,3,0,0] [0,1,1,0,1,1,0,1]

checkValues vs = (calcSend vs) + (calcMore vs) == (calcMoney vs)

type Incrementor = Int -> Either Int Int

incrementors :: [Incrementor]
incrementors = [inc 1, inc 0, inc 0, inc 0, inc 1, inc 0, inc 0, inc 0]
    where
      inc d x | ix > 9 = Right d
              | otherwise = Left ix
          where
            ix = x + 1

increment :: [Int] -> [Int]
increment vs = inc $ zip vs incrementors
    where
      inc ((v, i):rest) = either (\x -> x:(map fst rest)) (\x -> x:(inc rest)) (i v)
      inc [] = []

--This gets one answer and then dies with a stack overflow
solve :: [[Int]]
solve = filter checkValues $ tryValues [2,0,0,0,1,0,0,0]
    where 
      tryValues [1,0,0,0,1,0,0,0] = []
      tryValues vs = vs:(tryValues $ increment vs)
      
{-- where

solve >>= \[s,e,n,d,m,o,r,y] -> ...

gives the correct digit for each letter above to solve the equation

SEND + MORE = MONEY --}

format vs = (show vs) ++ ": " ++ (send vs) ++ " + " ++ (more vs) ++ " = " ++ (money vs)
    where
      send = show.calcSend
      more = show.calcMore
      money = show.calcMoney

main = mapM_ (putStrLn.format) solve