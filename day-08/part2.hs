import Data.Map (singleton, findWithDefault, insertWith', empty, Map)
import Data.Foldable (foldr)
import Data.List (transpose)

splitComplete :: Integer -> String -> [String]
splitComplete _ ['\n'] = []
splitComplete _ [] = []
splitComplete size list = h:(splitComplete size t)
  where (h, t) = splitAt (fromInteger size) list

getWidth :: Integer
getWidth = 25

getHeight :: Integer
getHeight = 6

toPixel :: Char -> Char
toPixel c
  | c == '0' = 'X'
  | otherwise = ' '

main = do
  layers <- fmap (splitComplete (getWidth * getHeight)) getContents
  let image = map (toPixel . head) (map (filter (/='2')) (transpose layers))
  let rows = splitComplete getWidth image
  sequence (map print rows)
