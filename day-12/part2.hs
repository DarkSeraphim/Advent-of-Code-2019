import Helpers (split, readInteger)
import Data.List (length)
import Data.Set (empty, member, insert, singleton)
import Debug.Trace (trace)

type Vec3 = (Integer, Integer, Integer)
data Moon = Moon {velocity :: Vec3, position :: Vec3} deriving (Ord, Eq, Show)

parseNum :: String -> Integer
parseNum (_:'=':num) = readInteger num

parseMoon :: String -> Moon
parseMoon line = Moon (0, 0, 0) (x, y, z)
  where noang = tail (init line)
        (sx:sy:sz:_) = split ',' (filter (/=' ') noang)
        x = parseNum sx
        y = parseNum sy
        z = parseNum sz

getGravity :: Integer -> Integer -> Integer
getGravity compA compB
  | compA < compB = 1
  | compA == compB = 0
  | compA > compB = (-1)

-- Returns first moon, after applying gravity of second moon
applyGravity :: Moon -> Moon -> Moon
applyGravity moonA moonB = Moon (avx + dx, avy + dy, avz + dz) posA
  where (avx, avy, avz) = velocity moonA
        posA = position moonA
        (apx, apy, apz) = posA
        (bpx, bpy, bpz) = position moonB
        dx = getGravity apx bpx
        dy = getGravity apy bpy
        dz = getGravity apz bpz

sumVec3 :: Vec3 -> Vec3 -> Vec3
sumVec3 (a, b, c) (d, e, f) = (a + d, b + e, c + f)

energyVec3 :: Vec3 -> Integer
energyVec3 (a, b, c) = (abs a) + (abs b) + (abs c)

energy :: Moon -> Integer
energy moon = energyVel * energyPos
    where energyVel = energyVec3 (position moon)
          energyPos = energyVec3 (velocity moon)

applyVelocity :: Moon -> Moon
applyVelocity moon = Moon vel newPos
    where pos = position moon
          vel = velocity moon
          newPos = sumVec3 pos vel

simulate :: [Moon] -> [Moon]
simulate moons = newMoons
    where moonsWithGravity = map (\moon -> (foldl (\a b -> applyGravity a b) moon moons)) moons
          newMoons = map applyVelocity moonsWithGravity

runSimulation :: Integer -> [Moon] -> [Moon]
runSimulation 0 moons = moons
runSimulation i moons = runSimulation (i - 1) (simulate moons)

vi (x, y, z) i
  | i == 0 = x
  | i == 1 = y
  | i == 2 = z  

getAxis moons idx = map (\moon -> (vi (velocity moon) idx, vi (position moon) idx)) moons

isSame :: Integer -> [Moon] -> [Moon] -> Bool
isSame idx orig moons = (getAxis orig idx) == (getAxis moons idx)

computeCycle n moons orig idx
  | isSame idx orig newMoons = n
  | otherwise = computeCycle (n + 1) newMoons orig idx
  where newMoons = simulate moons

main = do
  moons <- fmap (map parseMoon) (fmap lines getContents)
  let axis = [0,1,2]
  let nums = map (\a -> computeCycle 1 moons moons a) axis
  -- let c = computeCycle 1 1 0 moons
  print nums
  print $ foldr1 lcm nums
