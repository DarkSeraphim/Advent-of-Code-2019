import Helpers (split, readInteger)

type Direction = (Integer, Integer)
type Segment = (Point, Point)
data Point = Point Integer Integer deriving (Show)-- Point x y

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

smallerFirst :: Segment -> Segment
smallerFirst (Point x1 y1, Point x2 y2)
  | x1 == x2 && y1 < y2 = (Point x1 y1, Point x2 y2)
  | x1 == x2 = (Point x2 y2, Point x1 y1)
  | x1 < x2 = (Point x1 y1, Point x2 y2)
  | otherwise = (Point x2 y2, Point x1 y1)

toSegments :: [Point] -> [Segment]
toSegments (a:b:[]) = [smallerFirst (a, b)]
toSegments (a:b:rest) = (smallerFirst (a, b)):toSegments(b:rest)

-- Segment should always have the smaller x first
doCross :: Segment -> Segment -> Bool
doCross (Point xa1 ya1, Point xa2 ya2) (Point xb1 yb1, Point xb2 yb2)
                 -- First check if the X fits, then check if the Y fits
  | xa1 == xa2 = (xb1 <= xa1 && xa1 <= xb2) && (ya1 <= yb1 && yb1 <= ya2) -- First point is vertical
  | xb1 == xb2 = (xa1 <= xb1 && xb1 <= xa2) && (yb1 <= ya1 && ya1 <= yb2) -- Second point is vertical
  | otherwise = False -- Neither is, assuming lines don't completely overlap

getCrossing :: (Segment, Segment) -> Point
getCrossing ((Point xa1 ya1, Point xa2 ya2), (Point xb1 yb1, Point xb2 yb2))
  | xa1 == xa2 = Point xa1 yb1
  | ya1 == ya2 = Point xb1 ya1

notOrigin :: Point -> Bool
notOrigin (Point x y) = x /= 0 || y /= 0

manhattan :: Point -> Point -> Integer
manhattan (Point xo yo) (Point xx yy) = (abs (xo - xx)) + (abs (yo - yy))

main = do
  pathA <- readLine
  pathB <- readLine
  let segmentsA = toSegments pathA
  let segmentsB = toSegments pathB
  let possibilities = [(a, b) | a <- segmentsA, b <- segmentsB, doCross a b]
  let crossings = filter notOrigin $ map getCrossing possibilities
  print $ minimum $ map (manhattan (Point 0 0)) crossings
