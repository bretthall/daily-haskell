{--

From Mensa's Genius Quiz-a-Day book, problem for November 16th

A word square is composed of four words that read the same across and down.
The first word of the square below has been filled in for you. Make up a
word square, using this as a start, in which you use, in total, four E's;
two each, S, O, L, N, D; and one each, A and P.

       S    O    L    E

       O

       L

       E

--}

import Data.List (permutations, intersperse)
import System.IO.Unsafe (unsafePerformIO)
import Data.Char (toUpper)

pool :: String
pool = "EEEESSOOLLNNDDAP"

type Square = [String]

--from the symmetry of the puzzle we only have nine slots to fill in
-- S O L E
--   ? ? ? 
--     ? ? 
--       ? 

poolLeft :: String
poolLeft = "EENNDDSAP"

-- We only have one of each of these letters so they must go on the diagonal
diags :: String
diags = "SAP"

--We have two of each of these left so they must go in the non-diagonal slots
--(there's no space to put two any of these letters on the diagonal)
nonDiags :: String
nonDiags  = "END"

firstLine :: String
firstLine = "SOLE"

--There's only 36 possibilities so just use lists, they'll be fast enough
possible :: [Square]
possible = [(firstLine:(secondLineAndRest dPerm ndPerm)) |   
            dPerm <- permutations diags,
            ndPerm <- permutations nonDiags]
      where
        secondLineAndRest (d:ds) (nd1:nd2:nd3:[]) = ([d, nd1, nd2]:(thirdLineAndRest ds nd3))
        thirdLineAndRest (d1:d2:[]) nd = ([d1, nd]:(lastLine d2))
        lastLine d = [d]:[]

--There's probably a cleaner way to do this, but this works
toWords :: Square -> [String]
toWords rows = (r 0):((e 0 1):(r 1)):((e 0 2):(e 1 2):(r 2)):((e 0 3):(e 1 3):(e 2 3):(r 3)):[]
    where
      r rowNum = rows !! rowNum
      e rowNum colNum = rows !! rowNum !! (colNum - rowNum)

outputWords :: [String] -> IO ()
outputWords = (mapM_ putStrLn).(map (intersperse ' '))

-- living dangerously: the IO below should be self contained enough to be OK
-- NOTE: the original 5desk.txt file doesn't contain "ends", which is a problem for this exercise so I added it
allowedWords :: [String]
allowedWords = unsafePerformIO $ do
                 text <- readFile "5desk.txt"
                 return $ filter ((`elem` "SOLE").head) $ map (map toUpper) $ filter ((== 4).length) $ lines text
              
squareOK :: Square -> Bool
squareOK = (all (`elem` allowedWords)).toWords

-- *Main> mapM_ outputWords $ map toWords $ filter squareOK possible
-- S O L E
-- O P E N
-- L E A D
-- E N D S

-- Don't have time to generalize this right now, but to do it we would need to first remove all the 
-- letters from the given first row from the pool and then analyze whats left. Any single letters go
-- in the diagonal pool and any double letters go in the nondiagonal pool (once for each pair in the 
-- original pool). Note that if we don't have enough letters in the diagonal pool to fill the diagonal 
-- then we must have space on the diagonal for pairs of letters from the non-diagonal pool. Modifcations 
-- woudl need to be made above to take this into account. Then just proceed as above and hope that 5desk 
-- doesn't require any more additions to solve the puzzle.
wordSquare :: String -> Square -> [Square]
wordSquare pool structure = undefined

