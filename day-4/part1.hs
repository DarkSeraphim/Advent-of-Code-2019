import Helpers (split, readInteger)

splitNum :: Integer -> [Integer]
splitNum 0 = []
splitNum num = (splitNum (num `div` 10)) ++ [num `mod` 10]

isAscending :: [Integer] -> Bool
isAscending (a:b:c) = a <= b && isAscending (b:c)
isAscending _ = True -- default case

hasDuplicateNeighbours :: [Integer] -> Bool
hasDuplicateNeighbours (a:b:c) = a == b || hasDuplicateNeighbours (b:c)
hasDuplicateNeighbours _ = False -- default case

isSolution :: Integer -> Bool
isSolution num = (isAscending nums) && (hasDuplicateNeighbours nums)
    where nums = splitNum num

main = do
  splt <- fmap (split '-') getLine
  let (s:e:_) = map readInteger splt
  print $ length $ filter isSolution [s..e] 
  
