import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
{--

So, yesterday http://lpaste.net/119228 we had an inelegancy, and that was, 
I was counting triangles that were, in fact, not triangles at all, but were 
just simply lines.

My solution was to have a 'allInTheSameLine' function but that was contrived.
Why? What if our data set is so large as to make that hand-coded function
impractical? or incomplete? (or both)? Or, what if our data set was such that
it is not easily determined manually if several points are along the same line
(after all, what is 'line')? For example (and here we get into the 'line'-
definition), with geocentric or geodetic data, line is a slippery thing, that
is not easily (nor correctly) determined by following a straight-edge.

But they can be determined functionally.

So.

From yesterday's data set of vertices and their locations, however you defined
them (in Data.Graph or otherwise), write a function that from that data set
determines if three points are along the same line, or not.

It should work for any extention of that data set, as well, so, if vertices
are added later, the function should give the correct answer without it being
changed.

Okay, there it is. I'll provide the data set of vertices and their locations
the way I did it (a la Data.Graph), so you MAY base your solution off of that,
OR you MAY use your structure and types to provide your solution.

Recall that:

*Algorithmic.Automata.Cellular> oohPretty (genRule 90) seed 16 20
                      9
5 ____________________*_______________________________________
4 ___________________*_*______________________________________
3 __________________*___*_____________________________________
2 _________________*_*_*_*____________________________________
1 _____________6__*_______*__B________________________________
0 _______________*_*_____*_*__________________________________
9 ______________*___*___*___*_________________________________
8 _____________*_*_*_*_*_*_*_*________________________________
7 _________4__*_______8_______*__D____________________________
6 ___________*_*_____________*_*______________________________
5 __________*___*___________*___*_____________________________
4 _________*_*_*_*_________*_*_*_*____________________________
3 _____2__*_______*__5__A_*_______*__E________________________
2 _______*_*_____*_*_____*_*_____*_*__________________________
1 ______*___*___*___*___*___*___*___*_________________________
0 __1__*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*__F_____________________
       0123456789012345678901234567890
              3       7       C      

 --}

type Vertex = Node

data EdgeType = LP | RP | LC | RC | LS | RS

sierGraph :: Gr () EdgeType
sierGraph = mkGraph (map n [1..15]) [LEdge 1 2 RP, LEdge 1 3 RS, 
                                     LEdge 2 1 LC, LEdge 2 3 RC, LEdge 2 4 RP, LEdge 2 5 RS,
                                     LEdge 3 1 LS, LEdge 3 2 LP, LEdge 3 5 RP, LEdge 3 7 RS,
                                     LEdge 4 2 LC, LEdge 4 5 RC, LEdge 4 6 RP, LEdge 4 8 RS,
                                     LEdge 5 2 LS, LEdge 5 3 LC, LEdge 5 4 LP, LEdge 5 7 RC,
                                     LEdge 6 4 LC, LEdge 6 8 RC, LEdge 6 9 RP, LEdge 6 11 RS,
                                     LEdge 7 3 LS, LEdge 7 5 LP, LEdge 7 10 RP, LEdge 7 12 RS,
                                     LEdge 8 4 LS, LEdge 8 6 LP, LEdge 8 11 RP, LEdge 8 13 RS,
                                     LEdge 9 6 LC, LEdge 9 11 RC
                                     LEdge 10 7 LC, LEdge 10 12 RC, LEdge 10 13 RP, LEdge 10 14 RS,
                                     LEdge 11 6 LS, LEdge 11 8 LC, LEdge 11 9 LP, LEdge 11 13 RC,
                                     LEdge 12 7 LS, LEdge 12 10 LP, LEdge 12 14 RP, LEdge 12 15 RS,
                                     LEdge 13 8 LS, LEdge 13 10 LC, LEdge 13 11 LP, LEdge 13 14 RC,
                                     LEdge 14 10 LS, LEdge 14 12 LC, LEdge 14 13 LP, LEdge 14 15 RC,
                                     LEdge 15 12 LS, LEdge 15 14 LP]
                                     

allInTheSameLine :: Vertex -> Vertex -> Vertex -> Bool
allInTheSameLine a b c = undefined

{--

So:

*Main> allInTheSameLine 1 2 4 ~> True
*Main> allInTheSameLine 1 3 7 ~> True
*Main> allInTheSameLine 1 2 9 ~> True
*Main> allInTheSameLine 1 4 7 ~> False

--}