import Helpers (split, readInteger)

type Vec3 = (Integer, Integer, Integer)
data Moon = Moon {velocity :: Vec3, position :: Vec3}

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

main = do
  moons <- fmap (map parseMoon) (fmap lines getContents)
  let after = runSimulation 1000 moons
  print $ foldr (\a b -> a + b) 0 (map energy after)
