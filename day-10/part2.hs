import Debug.Trace (trace)
import Data.List (sortBy)
import Data.Map (empty, findWithDefault, insert, Map, toList, insertWith', size, (!))
import qualified Data.Foldable as F (foldr, foldl)


type Coord = (Integer, Integer)
data Point = Point Coord Char deriving (Eq, Show, Ord)
type Fract = (Integer, Integer)

-- 0 = top right, 1 = bottom right, 2 = bottom left, 3 = top left
getQuad :: Coord -> Coord -> Integer
getQuad (oy, ox) (y, x)
  | dy >= 0 && dx >= 0 = 0
  | dy < 0 && dx >= 0 = 1
  | dy < 0 && dx < 0 = 2
  | dy >= 0 && dx < 0 = 3
  where dy = ((-y) + oy)
        dx = x - ox

computeAngle2 :: Coord -> Point -> Float
computeAngle2 origin (Point c _) = computeAngle origin c

computeAngle :: Coord -> Coord -> Float
computeAngle origin coord
  | quad == 0 && dx == 0 = 0
  | quad == 0 && dy == 0 = (pi / 2)
  | quad == 0 = (pi / 2) - atan (dy / dx)
  | quad == 1 && dx == 0 = (pi / 2)
  | quad == 1 && dy == 0 = pi
  | quad == 1 = (pi / 2) - atan (dy / dx)
  | quad == 2 && dx == 0 = pi
  | quad == 2 && dy == 0 = 1.5 * pi
  | quad == 2 = (1.5 * pi) - atan (dy / dx)
  | quad == 3 && dx == 0 = 2 * pi
  | quad == 3 && dy == 0 = 1.5 * pi
  | quad == 3 = (1.5 * pi) - atan (dy / dx)
  where quad = getQuad origin coord
        (oy, ox) = origin
        (y, x) = coord
        dy = fromIntegral ((-y) + oy)
        dx = fromIntegral (x - ox)

clockwiseSort :: Coord -> Point -> Point -> Ordering
clockwiseSort origin (Point ca _) (Point cb _)
  | ca == cb = EQ -- Prevent floating point mishaps, do check on point
  | angleA < angleB = LT
  | otherwise = GT
  where angleA = computeAngle origin ca
        angleB = computeAngle origin cb

type VisMap = Map Fract Point
type DirMap = Map Point VisMap

createPoint :: Integer -> (Integer, Char) ->Point
createPoint y (x, legend) = Point (y, x) legend

bindRow :: (Integer, String) -> [Point]
bindRow (rowNum, row) = map (createPoint rowNum) (zip [0..] row)

isAstroid :: Point -> Bool
isAstroid (Point _ legend) = legend == '#'

fromFract :: Integer -> Integer -> (Integer, Integer)
fromFract dx dy = (dy `div` x, dx `div` x)
  where x = gcd dx dy

getDirection :: Coord -> Coord -> Fract
getDirection (ya, xa) (yb, xb) 
 | dx == 0 = ((signum dy), 0)
 | dy == 0 = (0, (signum dx))
 | otherwise = fromFract dx dy
  where dx = xb - xa
        dy = yb - ya

manhattan (ya, xa) (yb, xb) = dx + dy
  where dx = abs (xb - xa)
        dy = abs (yb - ya)

upsert :: Coord -> Point -> Point -> Point
upsert origin pa pb
  | da < db = pa
  | otherwise = pb
  where (Point ca _) = pa
        (Point cb _) = pb
        da = manhattan origin ca
        db = manhattan origin cb

recordDirection :: DirMap -> (Point, Point) -> DirMap
recordDirection map (p1, p2) = insert p1 newMap map
  where (Point c1 _) = p1
        (Point c2 _) = p2
        dir = getDirection c1 c2
        oldMap = findWithDefault empty p1 map
        newMap = insertWith' (upsert c1) dir p2 oldMap

findMax :: (Point, VisMap) -> (Point, VisMap) -> (Point, VisMap)
findMax best other
  | bestLength > otherLength = best
  | otherwise = other
  where (_, bestSet) = best
        (_, otherSet) = other
        bestLength = size bestSet
        otherLength = size otherSet

main = do
  rows <- fmap lines getContents
  let grid = mconcat $ map bindRow (zip [0..] rows)
  let astroids = filter isAstroid grid
  let couples = [(a, b) | a <- astroids, b <- astroids, a /= b]
  let anglesPerAstroid = F.foldl recordDirection empty couples
  let worst = (Point (-1, -1) 'x', empty)
  let ((Point origin _), others) = F.foldr findMax worst (toList anglesPerAstroid)
  let astroids2 = sortBy (clockwiseSort origin) (map (\(k, v) -> v) (toList others))
  print $ (astroids2 !! 199)
