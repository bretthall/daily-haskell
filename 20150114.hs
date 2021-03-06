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

data Division = Div {entry::Corner, square::Rect, rect1::(Rect, Division), rect2::(Rect, Division)}

instance Show Division where
    show (Div c s (r1, _) (r2, _)) = "Div " ++ show c ++ " " ++ show s ++ " " ++ show r1 ++ " " ++ show r2

p :: Double
p = 1.32471795724474602596
                 
divisions :: Length -> Division
divisions h = div initRect (C Lower Left)
    where
      initRect = Rect (P 0 0) (P (h*p) h)
      div (Rect (P x1 y1) (P x2 y2)) c@(C Lower Left) = Div c sqr (r1, div r1 (C Lower Right)) (r2, div r2 c)
          where
            h = y2 - y1
            w = x2 - x1
            h' = h / (1.0 + p)
            s = h' * p
            w' = h / p
            sqr = Rect (P (x1 + w') y1) (P x2 (y1 + s)) 
            r1 = Rect (P x1 y1) (P (x1 + w') y2)
            r2 = Rect (P (x1 + w') (y1 + s)) (P x2 y2)
      div (Rect (P x1 y1) (P x2 y2)) c@(C Lower Right) = Div c sqr (r1, div r1 (C Upper Right)) (r2, div r2 c)
          where
            h = y2 - y1
            w = x2 - x1
            w' = w / (1.0 + p)
            s = w' * p
            h' = w / p
            sqr = Rect (P (x1 + w') (y1 + h')) (P x2 y2) 
            r1 = Rect (P x1 y1) (P x2 (y2 + h'))
            r2 = Rect (P x1 (y1 + h')) (P (x1 + w') y2)
      div (Rect (P x1 y1) (P x2 y2)) c@(C Upper Right) = Div c sqr (r1, div r1 (C Upper Left)) (r2, div r2 c)
          where
            h = y2 - y1
            w = x2 - x1
            h' = h / (1.0 + p)
            s = h' * p
            w' = h / p
            sqr = Rect (P x1 (y1 + h')) (P (x1 + s) y2)
            r1 = Rect (P (x1 + s) y1) (P x2 y2)
            r2 = Rect (P x1 y1) (P (x1 + s) (y1 + h'))
      div (Rect (P x1 y1) (P x2 y2)) c@(C Upper Left) = Div c sqr (r1, div r1 (C Lower Left)) (r2, div r2 c)
          where
            h = y2 - y1
            w = x2 - x1
            w' = w / (1.0 + p)
            s = w' * p
            h' = w / p
            sqr = Rect (P x1 y1) (P (x1 + s) (y1 + s)) 
            r1 = Rect (P x1 (y1 + s)) (P x2 y2)
            r2 = Rect (P (x1 + s) y1) (P x2 (y1 + s))

generations :: Division -> [[Division]]
generations d = [d]:(next [d])
    where
      next ds = ds':(next ds')
          where 
            ds' = concatMap (\d -> [(snd.rect1) d, (snd.rect2) d]) ds
      

harrissSpiral :: Dimensions -> [Point]
harrissSpiral rectangle = undefined

-- this is all very ... fractal-y! FUN!