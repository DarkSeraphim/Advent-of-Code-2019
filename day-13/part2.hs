import IntcodeComputer (Memory, StdIO, preprocess, waitForExit, loadProgram, cin, cout)
import Data.Map (Map, empty, insert, elems)
import Control.Concurrent.Chan (writeList2Chan)

type Coord = (Integer, Integer)
type Tile = Integer
type Grid = Map Coord Tile

drawScreen :: Grid -> [Integer] -> IO Grid
drawScreen grid [] = return grid

drawScreen grid ((-1):0:t:r) = do
    print t
    drawScreen grid r

drawScreen grid (x:y:t:r) = do
    drawScreen newGrid r
    where newGrid = insert (x,y) t grid

main = do
  program <- loadProgram
  let freeProgram = insert 0 2 program
  stdio <- preprocess freeProgram
  writeList2Chan (cin stdio) 
  screen <- waitForExit stdio
  grid <- drawScreen empty screen
  print $ length (filter (==2) (elems grid))
