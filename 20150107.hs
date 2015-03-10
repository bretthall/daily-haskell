{--

So, here we are fa-la-la, tra-la-la, and I pull from my bundle of tricks
a math-ish problem.

Counting is 'math'-...ish, isn't it?

This is from @jammestanton who asks us to make the following observation
about the Fibonacci numbers, which we found from our dear sweetie, Rosalind
have to do with bunnies and what bunnies like to do.

So, we have the Fibonacci number series: 1,1,2,3,5,8,...

So let f n be the nth Fibonacci number so f 1 == 1, f 2 == 1, etc.

--}

fibs :: [(Integer, Integer)]
fibs = (1, 1):(rest 2 0 1)
    where 
      rest n m2 m1 = (n, m1 + m2):(rest (n + 1) m1 (m2 + m1))

f :: Integer -> Integer
f n = snd $ fibs !! (fromIntegral n)

-- Now, let g n be the count of the Fibonacci numbers < n (so, g 3 == 3)

g :: Integer -> Integer
g n = calc 0 fibs
    where 
      calc t (f:fs) | (snd f) < n = calc (t + 1) fs
                    | otherwise = t

-- NOW, let h n be the count of the number of g's that are < n

h :: Integer -> Integer
h n = calc 0 (map g [1..])
      where 
      calc t (g:gs) | g < n = calc (t + 1) gs
                    | otherwise = t

-- James asks: What do you notice unique to Fibonacci numbers?

-- *Main> map h [1..20]
-- [1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765]
-- *Main> map snd $ take 20 fibs
-- [1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765]
-- *Main> (map snd $ take 20 fibs) == (map h [1..20])
-- True
-- => h == f