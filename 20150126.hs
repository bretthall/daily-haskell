{-# LANGUAGE  TemplateHaskell, Rank2Types#-}

import Data.Array.IArray
import Control.Lens

{--

Okay, we'll start off simply this week with Mensa Genuis Quiz-a-Day
Book problem from July 22nd:

How many triangles are in this drawing?

*Algorithmic.Automata.Cellular> oohPretty (genRule 90) seed 16 20
U ____________________*_______________________________________
U ___________________*_*______________________________________
U __________________*___*_____________________________________
U _________________*_*_*_*____________________________________
U ________________*_______*___________________________________
U _______________*_*_____*_*__________________________________
U ______________*___*___*___*_________________________________
U _____________*_*_*_*_*_*_*_*________________________________
U ____________*_______________*_______________________________
U ___________*_*_____________*_*______________________________
U __________*___*___________*___*_____________________________
U _________*_*_*_*_________*_*_*_*____________________________
U ________*_______*_______*_______*___________________________
U _______*_*_____*_*_____*_*_____*_*__________________________
U ______*___*___*___*___*___*___*___*_________________________
U _____*_*_*_*_*_*_*_*_*_*_*_*_*_*_*_*________________________

--}

countingTriangles :: Graph -> Int
countingTriangles triangles = undefined

type Graph = Array Int Vertex
data Vertex = V {_idx::Int, _leftChild::Maybe Int, _rightChild::Maybe Int, _leftSibling::Maybe Int} deriving Show
$(makeLenses ''Vertex)

verts :: Array Int Vertex
verts = listArray (1,15) [
 (V 1 (Just 2) (Just 3) Nothing),
 (V 2 (Just 4) (Just 5) (Just 3)),
 (V 3 (Just 5) (Just 6) Nothing),
 (V 4 (Just 7) (Just 8) (Just 5)),
 (V 5 Nothing Nothing (Just 6)),
 (V 6 (Just 9) (Just 10) Nothing),
 (V 7 (Just 11) (Just 12) (Just 8)),
 (V 8 (Just 12) (Just 13) Nothing),
 (V 9 (Just 13) (Just 14) (Just 10)),
 (V 10 (Just 14) (Just 15) Nothing),
 (V 11 Nothing Nothing (Just 12)),
 (V 12 Nothing Nothing (Just 13)),
 (V 13 Nothing Nothing (Just 14)),
 (V 14 Nothing Nothing (Just 15)),
 (V 15 Nothing Nothing Nothing)]

type EdgeLength = Int
followEdge :: EdgeLength -> Vertex -> Lens' Vertex (Maybe Int) -> Maybe Int
followEdge l v b | l > 0 = case v^.b of
                             Just i -> followEdge (l - 1) (verts ! i) b
                             Nothing -> Nothing
                 | l == 0 = Just $ v^.idx

countTris :: Vertex -> Int
countTris v = foldr (+) 0 $ map (\l -> tryLeft l + trySibling l) possibleEdgeLengths
    where
      possibleEdgeLengths = [1,2,4]
      tryLeft :: EdgeLength -> Int
      tryLeft l = case tryLeft' l of 
                    Just _ -> 1
                    Nothing -> 0
      tryLeft' l = do
        pl1 <- followEdge l v leftChild
        pl2 <- followEdge l (verts ! pl1) leftSibling
        pr <- followEdge l v rightChild
        if pl2 == pr
        then Just pr
        else Nothing
      trySibling :: EdgeLength -> Int
      trySibling l = case trySibling' l of 
                    Just _ -> 1
                    Nothing -> 0
      trySibling' l = do
        ps1 <- followEdge l v leftSibling
        ps2 <- followEdge l (verts ! ps1) leftChild
        pr <- followEdge l v rightChild
        if ps2 == pr
        then Just pr
        else Nothing

-- *Main> foldr (+) 0 $ map (\i -> countTris $ verts ! i) [1..15]
-- 17

{-- Hint: I just used the cellular automata library to generate the triangles
for display here. That library is not part of the problem nor solution.

Importing and using Data.Graph, however, is, AND this week we're going to be
exploring vertices, geocentric and geodetic data, so getting comfy with
Data.Graph (again) might help not-so-much today, but perhaps later on this
week. FYI.

So. The hint is this: how to represent the vertices and edges of the above
Sierpinski triangle as Haskell data (perhaps even using Data.Graph), and then,
given that representation, how to count ALL the triangles in the above
triangle. --}

--BLAH, couldn't figure out how to use Data.Graph this quick enough, maybe if I had a few hours for this...

-- sierpinski :: Graph
-- sierpinski = buildG (1, 15) $ concat edges 
--     where 
--       tri (a, b, c) = [(a,b), (a,c), (b, c)]
--       --possible to have 1, 2, and 4 length triangles
--       edges1 = concatMap tri [(1,2,3), (2,4,5), (3,5,6), (4,7,8), (7,9,10), (8,10,11), (6,12,13), (12,11,14), (13,14,15)]
--       edges2 = concatMap tri [(1,4,6), (4,9,11), (6,11,15)]
--       edges3 = tri (1,9,15)


