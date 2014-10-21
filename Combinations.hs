module Combinations where

--generates all the combinations of the given number of elements from the given list
combinations :: Int -> [a] -> [[a]]
combinations n as = combinations' (length as) n as
    where 
      combinations' _ 1 as = map (:[]) as
      combinations' l n v@(a:as) | l > n = (map (a:) (combinations' (l-1) (n-1) as)) ++ (combinations' (l-1) n as)
                                 | otherwise = [v]
