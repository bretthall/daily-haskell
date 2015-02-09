{--

Solution to @1HaskellADay http://lpaste.net/120196

From Mensa Genuis Quiz-a-Day Book, February 9th:

The following addition example uses letters instead of numbers. Each letter
must be replaced with a number -- the same number each time the letter appears.
The puzzle will then be correct, mathematically.

                  OH
                  OH
                  OH
                  OH
                 ---
                  NO

[ed: in the original problem, 'NO' has an exclamation mark! Does this make
a difference to the solution?]

--}

ohno :: [(Int, Int, Int)]
ohno =  [(o, h, n) | o <- [0..9], h <- [0..9], n <- [0..9], 4*(o*10 + h) == n*10 + o]

ohnoExclaim = [(o, h, n, e) | o <- [0..9], h <- [0..9], n <- [0..9], e <- [0..9], 4*(o*10 + h) == n*100 + o*10 + e]

{--
*Main> [(o, h, n) | o <- [0..9], h <- [0..9], n <- [0..9], 4*(o*10 + h) == n*10 + o]
[(0,0,0),(0,5,2),(2,3,9)]
*Main> [(o, h, n, e) | o <- [0..9], h <- [0..9], n <- [0..9], e <- [0..9], 4*(o*10 + h) == n*100 + o*10 + e]
[(0,0,0,0),(0,1,0,4),(0,2,0,8),(3,3,1,2),(3,4,1,6),(6,5,2,0),(6,6,2,4),(6,7,2,8),(9,8,3,2),(9,9,3,6)]
--}