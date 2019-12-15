import IntcodeComputer (Memory, StdIO, preprocess, waitForExit, loadProgram)
import Data.Map (Map, empty, insert, keys, elems)

type Coord = (Integer, Integer)
type Tile = Integer
type Grid = Map Coord Tile

drawScreen :: Grid -> [Integer] -> Grid
drawScreen grid [] = grid
drawScreen grid (x:y:t:r) = drawScreen newGrid r
  where newGrid = insert (x,y) t grid

main = do
  program <- loadProgram
  stdio <- preprocess program
  screen <- waitForExit stdio
  let grid = drawScreen empty screen
  let minx = minimum (map (\(x, y) -> x) (keys grid))
  let miny = minimum (map (\(x, y) -> y) (keys grid))
  let maxx = maximum (map (\(x, y) -> x) (keys grid))
  let maxy = maximum (map (\(x, y) -> y) (keys grid))
  print $ "Minimum: " ++ (show minx) ++ ", " ++ (show miny)
  print $ "Maximum: " ++ (show maxx) ++ ", " ++ (show maxy)
  print $ length (filter (==2) (elems grid))
