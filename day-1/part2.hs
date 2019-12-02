readInteger :: String -> Integer
readInteger = read

computeFuel :: Integer -> Integer
computeFuel = (subtract 2) . (\x -> x `div` 3)

computeFuelForWeight :: Integer -> Integer
computeFuelForWeight weight
    | weight <= 8 = 0
    | otherwise = fuel + computeFuelForWeight fuel
        where fuel = computeFuel weight

computeModule :: Integer -> Integer
computeModule = computeFuelForWeight

main = do
  contents <- getContents
  let modules = map readInteger (lines contents)
  print $ foldr (+) 0 $ map computeModule modules
