readInteger :: String -> Integer
readInteger = read

computeFuel :: Integer -> Integer
computeFuel = (subtract 2) . (\x -> x `div` 3)

computeModule :: Integer -> Integer
computeModule = computeFuel

main = do
  contents <- getContents
  let modules = map readInteger (lines contents)
  print $ foldr (+) 0 $ map computeModule modules
