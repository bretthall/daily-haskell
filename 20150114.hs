{--

Today is a drawing exercise.

Read the article:

http://www.theguardian.com/science/alexs-adventures-in-numberland/2015/jan/13/golden-ratio-beautiful-new-curve-harriss-spiral

about the Harriss Spiral.

In short, the Golden Ratio subdivides a rectangle into squares with a single
remaining rectangle (to subdivide further). The Harriss Spiral subdivides a
rectangle into smaller rectangles with a remaining square, and the smaller
rectangles are then subdivided again, giving the spiral.

1. Plot the Golden Ratio by subdividing a rectangle into squares, then put a
quarter circle in each square to have you your Golden Spiral. Use your
favorite plotter (even if it's a pad and paper)

--}
import Prelude hiding (Left, Right, length)
import System.IO (hPutStrLn, Handle)

data Point = P Double Double
             deriving Show

type Length = Double
type Width = Double

data Dimensions = D Length Width
   deriving Show

data LeftRight = Left | Right
   deriving Show
data UpperLower = Upper | Lower
   deriving Show
data Corner = C UpperLower LeftRight
   deriving Show

goldenRatio = (1.0 + sqrt 5.0) / 2.0

makeGoldenRect :: Width -> Dimensions
makeGoldenRect w = D (w * goldenRatio) w

makeGRFromWidth :: Width -> Dimensions
makeGRFromWidth w = D (w * goldenRatio) w

makeGRFromLength :: Length -> Dimensions
makeGRFromLength l = D l (l * goldenRatio)

isGoldenRect :: Dimensions -> Bool
isGoldenRect (D l w) = isGood l w || isGood w l
    where
      isGood x y = abs (x / y - goldenRatio)/x < 0.1 

--Note: this method loses its 
goldenSpiral' :: Dimensions -> [(Corner, Point, Point, Dimensions)]
goldenSpiral' rectangle | isGoldenRect rectangle = start rectangle
                        | otherwise = undefined
    where
      start r@(D l w) | l > w = next (C Upper Left) (P 0 l) r
                      | otherwise = next (C Lower Left) (P 0 0) r
      -- next c p d | isGoldenRect d = next' c p d
      --            | otherwise = []
      next c p d = next' c p d
      next' c@(C Upper Left) p@(P x y) d@(D l w) = 
          (c, p, P (x + w) (y - w), d):(next (C Upper Right) (P (x + w) (y - w)) (makeGRFromLength (l - w)))
      next' c@(C Upper Right) p@(P x y) d@(D l w) = 
          (c, p, P (x - l) (y - l), d):(next (C Lower Right) (P (x - l) (y - l)) (makeGRFromWidth (w - l)))
      next' c@(C Lower Right) p@(P x y) d@(D l w) = 
          (c, p, P (x - w) (y + w), d):(next (C Lower Left) (P (x - w) (y + w)) (makeGRFromLength (l - w)))
      next' c@(C Lower Left) p@(P x y) d@(D l w) = 
          (c, p, P (x + l) (y + l), d):(next (C Upper Left) (P (x + l) (y + l)) (makeGRFromWidth (w - l)))

goldenSpiral :: Dimensions -> [Point]
goldenSpiral d = map (\(_, _, p, _) -> p) $ goldenSpiral' d

writePoints :: Handle -> [Point] -> IO ()
writePoints h = mapM_ fmt
    where
      fmt (P x y) = hPutStrLn h $ show x ++ "\t" ++ show y

-- you probably want to take x of the above, because show is non-terminating,
-- right?

{--

2. Plot the Harriss Spiral as described in the referenced article. Show your
results.

--}

data Rect = Rect {lowerLeft::Point, upperRight::Point} deriving Show
data Division = Div {square::Rect, entry::Corner, big::Division, small::Division} deriving Show

p :: Double
p = 1.32471795724474602596

harrissDivisions :: Length -> Division
harrissDivisions l = hDiv initRect (C Lower Left)
    where
      initRect = Rect (P 0 0) (P l*p l)
      hDiv (Rect (P x1 y1) (P x2 y2)) c@(C Lower Left) = Div sqr c big small
          where
            l = y2 - y1
            w = x2 - x1
            ls = l / (1.0 + p)
            s = ls * p
            wb = l / p
            sqr = Rect (P (x1 + wb) y1) (P (x1 + w) y1 + s) 
            big = vDiv (Rect (P x1 y1) (P (x1 + wb) y2)) (C Lower Right)
            small = hDiv (Rect (P (x1 + wb) (y1 + s)) (P x2, y2)) (C Lower Left)
      hDiv r (C Upper Right) = 

harrissSpiral :: Dimensions -> [Point]
harrissSpiral rectangle = undefined

-- this is all very ... fractal-y! FUN!