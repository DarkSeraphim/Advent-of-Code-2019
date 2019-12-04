import Helpers (split, readInteger)

splitNum :: Integer -> [Integer]
splitNum 0 = []
splitNum num = (splitNum (num `div` 10)) ++ [num `mod` 10]

isAscending :: [Integer] -> Bool
isAscending (a:b:c) = a <= b && isAscending (b:c)
isAscending _ = True -- default case

hasDuplicateNeighbours :: [Integer] -> Bool
hasDuplicateNeighbours (a:b:[]) = a == b
hasDuplicateNeighbours (a:b:c:[]) = a /= b && b == c
hasDuplicateNeighbours (a:b:c:d:e) = t || hasDuplicateNeighbours (b:c:d:e)
    where t = (a /= b && b == c && c /= d)
hasDuplicateNeighbours _ = False -- default case

hasDupes :: [Integer] -> Bool
hasDupes (a:b:c:d) = t || hasDuplicateNeighbours (a:b:c:d)
    where t = a == b && b /= c
          
hasDupes nums = hasDuplicateNeighbours nums

isSolution :: Integer -> Bool
isSolution num = (isAscending nums) && (hasDupes nums)
    where nums = splitNum num

main = do
  splt <- fmap (split '-') getLine
  let (s:e:_) = map readInteger splt
  print $ length $ filter isSolution [s..e] 
  
