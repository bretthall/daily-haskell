import Data.Graph
import Data.Array

{--

Solution to @1HaskellADay http://lpaste.net/119353

So, okay, YESterday we solved a problem, which is: when is a triangle a
triangle (and, specifically: when it's not a line. So there.)

Okay, fine.

TOday we solve a different problem. We have a bunch of vertices, as before
(and 'a bunch of' is a technical term), but, unlike before, where we were
provided with all the edges provided, NOW we are not provided ANY edges.

All vertices, no edges. No graph.

The PROBLEM-problem with this (not just the 'problem' but the 'PROBLEM-
problem') is that, surely, one can draw a bunch of lines (there's that
'a bunch of' term, again), but we are constrained in this universe of
triangles ...

(and WHY are we constrained? BECAUSE I SAID SO, THAT'S WHY!)

(okay, parental-scolding flashback *shudder*)

... in that no edge may cross over another edge. Edges may meet at a
vertex (and, in fact, to form triangles, they MUST meet at the vertices),
but they may not cross over at points between.

EVENTually, this will come to the question: "How many triangles can be
formed from a set of given vertices?" But that's EVENTually, NOT nOW!

(okay, @geophf, what's with the schizophrenic capitalization? I mean,
'leik: rEAALly!')

What we are concerning ourselves, here and now, is, okay, let's make
some triangles (and we know how do to that from before), but let's make
sure no edges cross each other.

Welp, how do we do that? Well, this is Haskell, right, so let's do it ...
functionally!

Uh. Leik. Yeah.

Yeah: leik, I said, leik, 'leik.' Deal.

SO! Given a set of vertices, and their associated locations, AND a
nascent graph (that has the current state of the edges up to now), have a
function, cross, that given two vertices, tells us if an edge between
them crosses over any of the existing edges. --}

type Point = (Int, Int)

-- We're looking things up by index all them time so I'm taking it easy on my pinky by switching to array so that I only 
-- have to type one ! when doing lookups
type PointArray = Array Vertex Point

cross :: Graph -> Vertex -> Vertex -> PointArray -> Bool
cross univ ptA ptB locs = any segmentsCross $ edges univ
    where
      p1 = locs ! ptA
      p2 = locs ! ptB
      -- check triangle orientations instead of doing divisions since division is slow (and we don't care about the actual
      -- location of the intersection, just that it exists)
      -- see http://www.geeksforgeeks.org/check-if-two-given-line-segments-intersect/ for details on this orientation business
      segmentsCross (vert1, vert2) = vertsOk && orientationsGood p1 p2 v1 v2 && orientationsGood v1 v2 p1 p2
          where
            -- if any of our vertices coincide then the line is OK (the orientation check will say different though because 
            -- the segments do intersect, it's just that we're OK with intersecting end points)
            vertsOk = ptA /= vert1 && ptA /= vert2 && ptB /= vert1 && ptB /= vert2
            v1 = locs ! vert1
            v2 = locs ! vert2
      orientationsGood p1 p2 p3 p4 = orientation p1 p2 p3 /= orientation p1 p2 p4

-- so, okay, how does this work?

{-- here is the data set I'm thinking of when I do this:

7                                            1
6                                     2  
5                          
4                           
3                               3          
2                                          
1                            4            
0                                  5
9                      6       
8                           
7                        
6                
5      
4      7
3
2
1
0                                       8

       0123456789012345678901234567890123456789

A pretty clear structure where we can see that if we have
the edges (1,3) and (5,7), then

cross graph 2 8 verts ~> True
cross graph 6 8 verts ~> True

but

cross graph 4 5 verts ~> False

--}

-- *Main> mapM_ print $ map (\(p1, p2) -> (p1, p2, cross ingress p1 p2 space)) [(i,j) | i <- [1..8], j <- [i..8], i /= j]
-- (1,2,False)
-- (1,3,False)
-- (1,4,False)
-- (1,5,False)
-- (1,6,False)
-- (1,7,False)
-- (1,8,False)
-- (2,3,False)
-- (2,4,True)
-- (2,5,True)
-- (2,6,False)
-- (2,7,False)
-- (2,8,True)
-- (3,4,False)
-- (3,5,False)
-- (3,6,False)
-- (3,7,False)
-- (3,8,True)
-- (4,5,False)
-- (4,6,False)
-- (4,7,False)
-- (4,8,True)
-- (5,6,False)
-- (5,7,False)
-- (5,8,False)
-- (6,7,False)
-- (6,8,True)
-- (7,8,False)
-- => Looks OK to me

space :: PointArray
--          1       2       3       4       5       6     7      8
space = listArray (1,8) [(39,17),(31,16),(26,13),(24,11),(28,10),(17,9),(0,4),(33,0)]

ingress :: Graph
ingress = buildG (1,8) [(1,3), (5,7)]

-- HINT: ------------------------------------------------------------

-- You MAY wish to use the alongTheSameLine work from yesterday, or
-- you MAY choose to ignore that work, and use the formula that derives
-- the point where two lines intersect, from, you know: math, and stuff.

-- ... 'stuff' is a technical term

x :: Point -> Int
x = fst

y :: Point -> Int
y = snd

data Orientation = Colinear | Clockwise | CounterClockwise deriving (Show, Eq)

--Calculates the orientation of the triangle formed by following a path around the three points in p1->p2->p3 order
orientation :: Point -> Point -> Point -> Orientation
orientation p1 p2 p3 | cross > 0 = CounterClockwise
                     | cross < 0 = Clockwise
                     | otherwise = Colinear
    where
      v1 = (x p2 - x p1, y p2 - y p1)
      v2 = (x p3 - x p2, y p3 - y p2)
      cross = x v1 * y v2 - y v1 * x v2


