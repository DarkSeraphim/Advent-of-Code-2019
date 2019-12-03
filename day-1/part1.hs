import Helpers (readInteger)

computeModuleFuel :: Integer -> Integer
computeModuleFuel = (subtract 2) . (\x -> x `div` 3)

main = do
  contents <- getContents
  let modules = map readInteger (lines contents)
  print $ foldr (+) 0 $ map computeModuleFuel modules
