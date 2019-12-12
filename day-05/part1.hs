import Data.Map (Map, insert, (!), fromList)
import Helpers (split, readInteger)

type IP = Integer
type Instruction = Integer
type Mode = Integer
type Memory = Map Integer Integer
type ParamMode = [Mode]

readParameter :: IP -> Integer -> Memory -> ParamMode -> Integer
readParameter ip i memory modes 
  | mode == 0 = memory ! value -- address mode
  | mode == 1 = value          -- immediate mode
    where mode = modes !! (fromIntegral i)
          value = memory ! (ip + i)

runAdd :: IP -> Memory -> ParamMode -> [Integer]
runAdd ip memory modes = run (ip + 4) newMemory
  where value1 = readParameter ip (toInteger 1) memory modes
        value2 = readParameter ip (toInteger 2) memory modes
        addr3 = memory ! (ip + 3)
        value3 = value1 + value2
        newMemory = insert addr3 value3 memory

runMult :: IP -> Memory -> ParamMode -> [Integer]
runMult ip memory modes = run (ip + 4) newMemory
  where addr3 = memory ! (ip + 3)
        value1 = readParameter ip (toInteger 1) memory modes
        value2 = readParameter ip (toInteger 2) memory modes
        value3 = value1 * value2
        newMemory = insert addr3 value3 memory

runRead :: IP -> Memory -> ParamMode -> [Integer]
runRead ip memory modes = run (ip + 2) newMemory
  where addr1 = memory ! (ip + 1)
        newMemory = insert addr1 1 memory -- Find a way to replace this with real input

runPrint :: IP -> Memory -> ParamMode -> [Integer]
runPrint ip memory modes = value : (run (ip + 2) newMemory)
  where addr1 = memory ! (ip + 1)
        value = memory ! addr1
        newMemory = memory

runExit :: IP -> Memory -> ParamMode -> [Integer]
runExit ip memory modes = []

parseMode :: Integer -> Integer -> Mode
parseMode i op = (op `div` (10 ^ (i + 1))) `mod` 10

-- ParamMode has -1 at index 0 as a dummy value,
-- parameter modes are always 1 indexed (as their memory offset is at IP + i)
parseModes :: Integer -> (Instruction, ParamMode)
parseModes op = (ins, [-1, param1, param2, param3])
    where ins = op `mod` 100
          param1 = parseMode 1 op
          param2 = parseMode 2 op
          param3 = parseMode 3 op
          

run :: IP -> Memory -> [Integer]
run ip memory
  | ins == 1 = runAdd ip memory modes
  | ins == 2 = runMult ip memory modes
  | ins == 3 = runRead ip memory modes
  | ins == 4 = runPrint ip memory modes
  | ins == 99 = runExit ip memory modes
    where
          op = memory ! ip
          (ins, modes) = parseModes op

preprocess :: Memory -> [Integer]
preprocess memory =
  run 0 memory

main = do
  commands <- fmap (map readInteger) (fmap (split ',') getLine)
  print $ preprocess (fromList (zip [0..] commands))
