import Helpers (readInteger)

computeFuelForWeight :: Integer -> Integer
computeFuelForWeight = (subtract 2) . (\x -> x `div` 3)

computeModuleFuel :: Integer -> Integer
computeModuleFuel weight
    | weight <= 8 = 0
    | otherwise = fuel + computeModuleFuel fuel
        where fuel = computeFuelForWeight weight

main = do
  contents <- getContents
  let modules = map readInteger (lines contents)
  print $ foldr (+) 0 $ map computeModuleFuel modules
