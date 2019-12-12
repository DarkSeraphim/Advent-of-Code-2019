import Helpers (split, readInteger)

type Direction = (Integer, Integer)
type Segment = (Point, Point, Integer) -- Start, End, Length
data Point = Point Integer Integer deriving (Show)-- Point x y
data Crossing = Crossing Point Integer Integer -- StepsA StepsB

addDirection :: Point -> Direction -> Point
addDirection (Point x y) (dx, dy) = Point (x + dx) (y + dy) 

parseDirection :: String -> Direction
parseDirection (direction:length)
  | direction == 'U' = (0, readInteger length)
  | direction == 'D' = (0, negate $ readInteger length)
  | direction == 'L' = (negate $ readInteger length, 0)
  | direction == 'R' = (readInteger length, 0)

toLine :: Point -> [Direction] -> [Point]
toLine point [] = [point]
toLine point (direction:rest) = point : (toLine nextPoint rest)
    where nextPoint = addDirection point direction

readLine :: IO [Point]
readLine = fmap (toLine (Point 0 0)) $ fmap (map parseDirection) $ fmap (split ',') getLine

manhattan :: Point -> Point -> Integer
manhattan (Point xo yo) (Point xx yy) = (abs (xo - xx)) + (abs (yo - yy))

toSegments :: Integer -> [Point] -> [Segment]
toSegments d (a:b:[]) = [(a, b, d)]
toSegments d (a:b:rest) = (a, b, d):(toSegments (d + (manhattan a b)) (b:rest))

smallerFirst :: Segment -> Segment
smallerFirst (Point x1 y1, Point x2 y2, d)
  | x1 == x2 && y1 < y2 = (Point x1 y1, Point x2 y2, d)
  | x1 == x2 = (Point x2 y2, Point x1 y1, d)
  | x1 < x2 = (Point x1 y1, Point x2 y2, d)
  | otherwise = (Point x2 y2, Point x1 y1, d)

doCross :: Segment -> Segment -> Bool
doCross segmentA segmentB
                 -- First check if the X fits, then check if the Y fits
  | xa1 == xa2 = (xb1 <= xa1 && xa1 <= xb2) && (ya1 <= yb1 && yb1 <= ya2) -- First point is vertical
  | xb1 == xb2 = (xa1 <= xb1 && xb1 <= xa2) && (yb1 <= ya1 && ya1 <= yb2) -- Second point is vertical
  | otherwise = False -- Neither is, assuming lines don't completely overlap
    where (Point xa1 ya1, Point xa2 ya2, _) = smallerFirst segmentA
          (Point xb1 yb1, Point xb2 yb2, _) = smallerFirst segmentB

getCrossing :: (Segment, Segment) -> Crossing
getCrossing ((Point xa1 ya1, Point xa2 ya2, da), (Point xb1 yb1, Point xb2 yb2, db))
  | xa1 == xa2 = Crossing (Point xa1 yb1) (da + (manhattan (Point xa1 ya1) (Point xa1 yb1))) (db + (manhattan (Point xb1 yb1) (Point xa1 yb1)))
  | ya1 == ya2 = Crossing (Point xb1 ya1) (da + (manhattan (Point xa1 ya1) (Point xb1 ya1))) (db + (manhattan (Point xb1 yb1) (Point xb1 ya1)))

notOrigin :: Crossing -> Bool
notOrigin (Crossing (Point x y) _ _) = x /= 0 || y /= 0

getSteps :: Crossing -> Integer
getSteps (Crossing (Point x y) da db) = da + db

main = do
  pathA <- readLine
  pathB <- readLine
  let segmentsA = toSegments 0 pathA
  let segmentsB = toSegments 0 pathB
  let possibilities = [(a, b) | a <- segmentsA, b <- segmentsB, doCross a b]
  let crossings = filter notOrigin $ map getCrossing possibilities
  print $ minimum $ map getSteps crossings
