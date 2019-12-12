import Debug.Trace (trace)
import Data.List (permutations)
import Data.Map (Map, insert, findWithDefault, fromList, (!))
import Helpers (split, readInteger)

type IP = Integer
type Instruction = Integer
type Mode = Integer
type Memory = Map Integer Integer
type Input = [Integer]
type ParamMode = [Mode]
type RelativeBase = Integer

readAddress :: IP -> Integer -> Memory -> RelativeBase -> ParamMode -> Integer
readAddress ip i memory base modes
  | mode == 0 = value
  | mode == 2 = value + base
    where mode = modes !! (fromIntegral i)
          value = findWithDefault 0 (ip + i) memory

readParameter :: IP -> Integer -> Memory -> RelativeBase -> ParamMode -> Integer
readParameter ip i memory base modes 
  | mode == 0 = findWithDefault 0 value memory -- address mode
  | mode == 1 = value          -- immediate mode
  | mode == 2 = findWithDefault 0 (value + base) memory
    where mode = modes !! (fromIntegral i)
          value = findWithDefault 0 (ip + i) memory

runAdd :: IP -> Memory -> RelativeBase -> Input -> ParamMode -> [Integer]
runAdd ip memory base input modes = run (ip + 4) newMemory base input
  where value1 = readParameter ip 1 memory base modes
        value2 = readParameter ip 2 memory base modes
        addr3 = readAddress ip 3 memory base modes
        value3 = value1 + value2
        newMemory = insert addr3 value3 memory

runMult :: IP -> Memory -> RelativeBase -> Input -> ParamMode -> [Integer]
runMult ip memory base input modes = run (ip + 4) newMemory base input
  where value1 = readParameter ip 1 memory base modes
        value2 = readParameter ip 2 memory base modes
        addr3 = readAddress ip 3 memory base modes
        value3 = value1 * value2
        newMemory = insert addr3 value3 memory

runRead :: IP -> Memory -> RelativeBase -> Input -> ParamMode -> [Integer]
runRead ip memory base input modes = run (ip + 2) newMemory base newInput
  where addr1 = readAddress ip 1 memory base modes
        (value:newInput) = input
        newMemory = insert addr1 value memory -- Find a way to replace this with real input

runPrint :: IP -> Memory -> RelativeBase -> Input -> ParamMode -> [Integer]
runPrint ip memory base input modes = value : (run (ip + 2) newMemory base input)
  where value = readParameter ip 1 memory base modes
        newMemory = memory

runJumpIfTrue :: IP -> Memory -> RelativeBase -> Input -> ParamMode -> [Integer]
runJumpIfTrue ip memory base input modes
  | condition == True = run value2 memory base input
  | otherwise = run (ip + 3) memory base input
  where value1 = readParameter ip 1 memory base modes
        value2 = readParameter ip 2 memory base modes
        condition = value1 /= 0

runJumpIfFalse :: IP -> Memory -> RelativeBase -> Input -> ParamMode -> [Integer]
runJumpIfFalse ip memory base input modes
  | condition == True = run value2 memory base input
  | otherwise = run (ip + 3) memory base input
  where value1 = readParameter ip 1 memory base modes
        value2 = readParameter ip 2 memory base modes
        condition = value1 == 0

runLessThan :: IP -> Memory -> RelativeBase -> Input -> ParamMode -> [Integer]
runLessThan ip memory base input modes = run (ip + 4) newMemory base input
  where value1 = readParameter ip 1 memory base modes
        value2 = readParameter ip 2 memory base modes
        addr3 = readAddress ip 3 memory base modes
        result = if value1 < value2 then 1 else 0
        newMemory = insert addr3 result memory

runEquals :: IP -> Memory -> RelativeBase -> Input -> ParamMode -> [Integer]
runEquals ip memory base input modes = run (ip + 4) newMemory base input
  where value1 = readParameter ip 1 memory base modes
        value2 = readParameter ip 2 memory base modes
        addr3 = readAddress ip 3 memory base modes
        result = if value1 == value2 then 1 else 0
        newMemory = insert addr3 result memory

runRelBase :: IP -> Memory -> RelativeBase -> Input -> ParamMode -> [Integer]
runRelBase ip memory base input modes = run (ip + 2) memory newBase input
  where value1 = readParameter ip 1 memory base modes
        newBase = base + value1

runExit :: IP -> Memory -> RelativeBase -> Input -> ParamMode -> [Integer]
runExit ip memory base modes input = []

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

run :: IP -> Memory -> RelativeBase -> Input -> [Integer]
run ip memory base input
  | ins == 1 = runAdd ip memory base input modes
  | ins == 2 = runMult ip memory base input modes
  | ins == 3 = runRead ip memory base input modes
  | ins == 4 = runPrint ip memory base input modes
  | ins == 5 = runJumpIfTrue ip memory base input modes
  | ins == 6 = runJumpIfFalse ip memory base input modes
  | ins == 7 = runLessThan ip memory base input modes
  | ins == 8 = runEquals ip memory base input modes
  | ins == 9 = runRelBase ip memory base input modes
  | ins == 99 = runExit ip memory base input modes
    where op = memory ! ip
          (ins, modes) = parseModes op

preprocess :: Memory -> Input -> [Integer]
preprocess memory input =
  run 0 memory 0 input

main = do
  commands <- fmap (map readInteger) (fmap (split ',') getLine)
  let memory = fromList (zip [0..] commands)
  print $ preprocess memory [2]
