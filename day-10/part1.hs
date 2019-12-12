import Debug.Trace (trace)
import qualified Data.Ratio as R ((%), numerator, denominator)
import Data.Map (empty, findWithDefault, insert, Map, toList, (!))
import qualified Data.Set as S (empty, insert, Set, size, toList) 
import qualified Data.Foldable as F (foldr, foldl)


type Coord = (Integer, Integer)
data Point = Point Coord Char deriving (Eq, Ord, Show)
type Fract = (Integer, Integer)

type DirMap = Map Point (S.Set Fract)

createPoint :: Integer -> (Integer, Char) ->Point
createPoint y (x, legend) = Point (y, x) legend

bindRow :: (Integer, String) -> [Point]
bindRow (rowNum, row) = map (createPoint rowNum) (zip [0..] row)

isAstroid :: Point -> Bool
isAstroid (Point _ legend) = legend == '#'

fromFract :: Integer -> Integer -> (Integer, Integer)
fromFract dx dy = (dy `div` x, dx `div` x)
  where x = gcd dx dy

getAngle :: Coord -> Coord -> Fract
getAngle (ya, xa) (yb, xb) 
 | dx == 0 = ((signum dy), 0)
 | dy == 0 = (0, (signum dx))
 | otherwise = fromFract dx dy
  where dx = xb - xa
        dy = yb - ya
        
recordAngle :: DirMap -> (Point, Point) -> DirMap
recordAngle map (p1, p2) = insert p1 newSet map
  where (Point c1 _) = p1
        (Point c2 _) = p2
        angle = getAngle c1 c2
        oldSet = findWithDefault S.empty p1 map
        newSet = S.insert angle oldSet

findMax :: (Point, S.Set Fract) -> (Point, S.Set Fract) -> (Point, S.Set Fract)
findMax best other
  | bestLength > otherLength = best
  | otherwise = other
  where (_, bestSet) = best
        (_, otherSet) = other
        bestLength = S.size bestSet
        otherLength = S.size otherSet

f (y, x) = (y + 8, x + 5)

main = do
  rows <- fmap lines getContents
  let grid = mconcat $ map bindRow (zip [0..] rows)
  let astroids = filter isAstroid grid
  let couples = [(a, b) | a <- astroids, b <- astroids, a /= b]
  let anglesPerAstroid = F.foldl recordAngle empty couples
  let worst = (Point (-1, -1) 'x', S.empty)
  let ((Point (y, x) _), others) = F.foldr findMax worst (toList anglesPerAstroid)
  let sol = S.size others
  print $ "Solution: " ++ (show x) ++ "," ++ (show y) ++ ": " ++ (show sol)
