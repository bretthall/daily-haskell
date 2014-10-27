import Combinations
import Control.Monad (sequence_)
import Data.List (intersperse, permutations)

{--

Day One (somebody should name a movie studio after that phrase!) of
Kakuro-week. So, the end goal is to be able to generate a Kakuro
puzzle from some particular domain model of your choosing, but let's
build that up one step at a time.

-- PROBLEMS --------------------------------------------------------

The first step. Kakuro is a game (that you can look up) where you
place different numbers along horizontal and vertical lines that
sum to their clues. Simple enough. So, if you're working with
single digits (which you are working with in Kakuro), 

P1. What is the largest number that can be represented? --}

maxSum :: [Int] -> Int
maxSum allDifferentDigitsGreaterThanZero = undefined

{--

Okay, fine. 

P2. Now, create a mini-summer that gives you options of
an unordered list of digits, given the number of slots the digits
may fill, and the sum they must total to: --}

digits :: [Int]
digits = [1..9]

options :: Int -> Int -> [[Int]]
options total numberSlots = filter ((total == ).sum) $ combinations numberSlots digits

{--

P3. Run options against the following (total, numberSlots) pairs: --}

down, across :: [(Int, Int)]
down = [(9, 2), (35, 6), (3, 2), (24, 3), (16, 2)]
across = [(17, 2), (4, 2), (23, 3), (15, 2)]

{-- What are the answers you get?

*Main> [(ps, (uncurry options) ps) | ps <- down]
[((9,2),[[1,8],[2,7],[3,6],[4,5]]),((35,6),[[1,4,6,7,8,9],[2,3,6,7,8,9],[2,4,5,7,8,9],[3,4,5,6,8,9]]),((3,2),[[1,2]]),((24,3),[[7,8,9]]),((16,2),[[7,9]])]
*Main> [(ps, (uncurry options) ps) | ps <- across]
[((17,2),[[8,9]]),((4,2),[[1,3]]),((23,3),[[6,8,9]]),((15,2),[[6,9],[7,8]])]

--}

{-- VISUAL --------------------------------------------------------}

{--

Let's start building what our puzzle will look like, you know, like,
on the screen, so you can show off to your friends, family, and significant
ones. You know: "Dear! Dear! Look what I did!" "That's, uh, nice."

V1. Create and display an nxm grid with all empty slots. You whatever
visual-representation language you like, be it HTML, PDF, VRML, whatever
you like. Send it on out to the screen, take a pic, and share with the
world. You are in business! --}

repeatIO :: Int -> IO () -> IO ()
repeatIO n = sequence_.(take n).repeat

displayEmptyGrid :: Int -> Int -> IO ()
displayEmptyGrid rows cols = displayGrid $ take rows $ repeat $ take cols $ repeat " "

displayGrid :: [[String]] -> IO ()
displayGrid rows = (sequence_ (map displayRow formattedRows)) >> rowCap
    where 
      colWidth = maximum $ map length $ concat rows
      numCols = maximum $ map length rows
      spaces n = concat $ take n $ repeat " "
      format s = (spaces (colWidth - (length s))) ++ s
      formattedRows = map formatRow rows
      formatRow row = (map format row) ++ (take (numCols - (length row)) $ repeat $ spaces colWidth)
      displayRow row = rowCap >> putStr "|" >> mapM_ putStr (intersperse "|" row) >> putStrLn "|"
      rowCap = (repeatIO numCols (putStr "+" >> repeatIO colWidth (putStr "-"))) >> putStrLn "+"
      
toString :: (Show a) => [[a]] -> [[String]]
toString = map (map show)

{--

V2. Simple-Doku

Okay, great, and empty grid. Now, having your grid representation, fill
the grid with digits, each row and each column must have all different
digits. Show one solution. --}

allUnique :: (Eq a) => [a] -> Bool
allUnique [_] = True
allUnique (x:xs) | x `elem` xs = False
                 | otherwise = allUnique xs

colsGood :: (Eq a) => [[a]] -> Bool
colsGood rows | length (head rows) > 0 = (allUnique $ map head rows) && (colsGood $ map tail rows)
              | otherwise = True

type Row = [Int]
possibleRows :: Int -> [Row]
possibleRows = (concatMap permutations).((flip combinations) digits)

type Grid = [Row]

addRows :: Grid -> [Row] -> [Grid]
addRows g (r:rs) = if (colsGood (r:g)) then ((r:g) ++ addRows g rs) else (addRows g rs)
addRows _ [] = []

possibleGrids :: Int -> Int -> [Grid]
possibleGrids rows cols = 

simpleDokuSolver :: Int -> Int -> IO ()
simpleDokuSolver rows cols = undefined

-- HAVE FUN! YAY!