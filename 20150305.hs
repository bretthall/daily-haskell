{--
Solution to @1HaskellADay http://lpaste.net/121797 by @bretthall

From Project Euler

Integer right triangles
Problem 39
If p is the perimeter of a right angle triangle with integral length sides,
{a,b,c}, there are exactly three solutions for p = 120.

{20,48,52}, {24,45,51}, {30,40,50}

For which value of p <= 1000, is the number of solutions maximised?

 --}

module Main where

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

type Perimeter = Int

type Triangle = (Int, Int, Int)

perimeter :: Triangle -> Perimeter
perimeter (x, y, z) = x + y + z

trianglesUpToPerimeter :: Perimeter -> [Triangle]
trianglesUpToPerimeter maxPerimeter = [(x, y, z) | 
                                       z <- [1..1000], 
                                       y <- [1..z], 
                                       x <- [1..y], 
                                       x + y + z <= maxPerimeter, 
                                       x*x + y*y == z*z]

trianglesWithPerimeter :: Perimeter -> [Triangle]
trianglesWithPerimeter p = [(x, y, z) | 
                            z <- [1..1000], 
                            y <- [1..z], 
                            x <- [1..y], 
                            x + y + z == p, 
                            x*x + y*y == z*z]

countPerimeters :: Perimeter -> V.Vector Int
countPerimeters maxPerimeter = V.create $ do
                                 ps <- MV.replicate (maxPerimeter + 1) 0
                                 mapM_ (incPerimeter ps) $ trianglesUpToPerimeter maxPerimeter
                                 return ps
    where
      incPerimeter ps t = do
        let i = perimeter t
        oldVal <- MV.read ps i
        MV.write ps i (oldVal + 1)

findPerimeterWithMaxtriangles :: V.Vector Int -> (Int, Perimeter)
findPerimeterWithMaxtriangles ps = (num, perim)
    where
      (num, perim, _) = V.foldl' checkMax (0, 0, 0) ps
      checkMax (n, p, i) n' = if n' > n then (n', i, i + 1) else (n, p, i + 1)

-- then with the above find the solution

--ghci takes too long so compile it
main = do
  let (num, p) = findPerimeterWithMaxtriangles $ countPerimeters 1000
  putStrLn $ "Perimeter with most triangles = " ++ show p          
  putStrLn $ show num ++ " triangles:"
  mapM_ print $ trianglesWithPerimeter p

-- Perimeter with most triangles = 840
-- 8 triangles:
-- (240,252,348)
-- (210,280,350)
-- (168,315,357)
-- (140,336,364)
-- (120,350,370)
-- (105,360,375)
-- (56,390,394)
-- (40,399,401)