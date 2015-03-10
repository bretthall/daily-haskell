{--

Solution to @1HaskellADay http://lpaste.net/120037

On This Day in Math ‏@OnThisDayinMath 3h3 hours ago
http://pballew.blogspot.com/2015/02/on-this-day-in-math-february-5.html
The 36th day of the year; 36 is the smallest non trivial number which 
is both triangular and square.

What's the next one? What's the next one after that? (etc., et al)

--}

import Data.List.Ordered (isect)

squares :: [Integer]
squares = map (^2) [1..]

triangles :: [Integer]
triangles = map (\n -> (n*(n+1)) `div` 2) [1..]

triangularSquares :: [Integer]
triangularSquares = isect squares triangles
 
nextTriangularSquareAfter :: Integer -> Integer
nextTriangularSquareAfter num = findIt triangularSquares
    where 
      findIt (t:ts) | t == num = head ts
                    | otherwise = findIt ts

-- e.g.: nextTriangularSquareAfter 1 ~> 36

{--
*Main> nextTriangularSquareAfter 1
36
*Main> nextTriangularSquareAfter it
1225
*Main> nextTriangularSquareAfter it
41616
*Main> nextTriangularSquareAfter it
1413721
*Main> nextTriangularSquareAfter it
48024900
*Main> nextTriangularSquareAfter it
1631432881
*Main> nextTriangularSquareAfter it
55420693056
*Main> nextTriangularSquareAfter it
1882672131025
 GHCI is starting to work really hard so we'll stop here
--}