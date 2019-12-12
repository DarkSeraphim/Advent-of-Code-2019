import Data.Map (singleton, findWithDefault, insertWith', empty, Map)
import Data.Foldable (foldr)

splitComplete :: Integer -> String -> [String]
splitComplete _ ['\n'] = []
splitComplete _ [] = []
splitComplete size list = h:(splitComplete size t)
  where (h, t) = splitAt (fromInteger size) list

getWidth :: Integer
getWidth = 25

getHeight :: Integer
getHeight = 6

frequency :: String -> Map Char Integer
frequency [] = empty
frequency (c:r) = newMap
  where oldMap = frequency r
        newMap = insertWith' (+) c 1 oldMap

getMinMap key mapA mapB 
  | freqA > freqB = mapB
  | otherwise = mapA
  where freqA = findWithDefault 0 key mapA
        freqB = findWithDefault 0 key mapB

main = do
  layers <- fmap (splitComplete (getWidth * getHeight)) getContents
  let least = foldr (getMinMap '0') (singleton '0' (getWidth * getHeight)) (map frequency layers)
  let one = findWithDefault 0 '1' least
  let two = findWithDefault 0 '2' least
  print (one * two)
  


