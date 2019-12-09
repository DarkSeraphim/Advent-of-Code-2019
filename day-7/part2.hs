import Data.List (permutations)
import Data.Map (Map, insert, (!), fromList)
import Helpers (split, readInteger)

type IP = Integer
type Instruction = Integer
type Mode = Integer
type Memory = Map Integer Integer
type Input = [Integer]
type ParamMode = [Mode]

readParameter :: IP -> Integer -> Memory -> ParamMode -> Integer
readParameter ip i memory modes 
  | mode == 0 = memory ! value -- address mode
  | mode == 1 = value          -- immediate mode
    where mode = modes !! (fromIntegral i)
          value = memory ! (ip + i)

runAdd :: IP -> Memory -> Input -> ParamMode -> [Integer]
runAdd ip memory input modes = run (ip + 4) newMemory input
  where value1 = readParameter ip 1 memory modes
        value2 = readParameter ip 2 memory modes
        addr3 = memory ! (ip + 3)
        value3 = value1 + value2
        newMemory = insert addr3 value3 memory

runMult :: IP -> Memory -> Input -> ParamMode -> [Integer]
runMult ip memory input modes = run (ip + 4) newMemory input
  where value1 = readParameter ip 1 memory modes
        value2 = readParameter ip 2 memory modes
        addr3 = memory ! (ip + 3)
        value3 = value1 * value2
        newMemory = insert addr3 value3 memory

runRead :: IP -> Memory -> Input -> ParamMode -> [Integer]
runRead ip memory input modes = run (ip + 2) newMemory newInput
  where addr1 = memory ! (ip + 1)
        (value:newInput) = input
        newMemory = insert addr1 value memory -- Find a way to replace this with real input

runPrint :: IP -> Memory -> Input -> ParamMode -> [Integer]
runPrint ip memory input modes = value : (run (ip + 2) newMemory input)
  where value = readParameter ip 1 memory modes
        newMemory = memory

runJumpIfTrue :: IP -> Memory -> Input -> ParamMode -> [Integer]
runJumpIfTrue ip memory input modes
  | condition == True = run value2 memory input
  | otherwise = run (ip + 3) memory input
  where value1 = readParameter ip 1 memory modes
        value2 = readParameter ip 2 memory modes
        condition = value1 /= 0

runJumpIfFalse :: IP -> Memory -> Input -> ParamMode -> [Integer]
runJumpIfFalse ip memory input modes
  | condition == True = run value2 memory input
  | otherwise = run (ip + 3) memory input
  where value1 = readParameter ip 1 memory modes
        value2 = readParameter ip 2 memory modes
        condition = value1 == 0

runLessThan :: IP -> Memory -> Input -> ParamMode -> [Integer]
runLessThan ip memory input modes = run (ip + 4) newMemory input
  where value1 = readParameter ip 1 memory modes
        value2 = readParameter ip 2 memory modes
        addr3 = memory ! (ip + 3)
        result = if value1 < value2 then 1 else 0
        newMemory = insert addr3 result memory

runEquals :: IP -> Memory -> Input -> ParamMode -> [Integer]
runEquals ip memory input modes = run (ip + 4) newMemory input
  where value1 = readParameter ip 1 memory modes
        value2 = readParameter ip 2 memory modes
        addr3 = memory ! (ip + 3)
        result = if value1 == value2 then 1 else 0
        newMemory = insert addr3 result memory

runExit :: IP -> Memory -> Input -> ParamMode -> [Integer]
runExit ip memory modes input = []

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

run :: IP -> Memory -> Input -> [Integer]
run ip memory input
  | ins == 1 = runAdd ip memory input modes
  | ins == 2 = runMult ip memory input modes
  | ins == 3 = runRead ip memory input modes
  | ins == 4 = runPrint ip memory input modes
  | ins == 5 = runJumpIfTrue ip memory input modes
  | ins == 6 = runJumpIfFalse ip memory input modes
  | ins == 7 = runLessThan ip memory input modes
  | ins == 8 = runEquals ip memory input modes
  | ins == 99 = runExit ip memory input modes
    where op = memory ! ip
          (ins, modes) = parseModes op

preprocess :: Memory -> Input -> [Integer]
preprocess memory input =
  run 0 memory input

runAmps :: Memory -> Input -> Integer
runAmps memory inputs = foldl (\lastOutput newInput -> last (preprocess memory [newInput, lastOutput])) 0 inputs

main = do
  commands <- fmap (map readInteger) (fmap (split ',') getLine)
  let memory = fromList (zip [0..] commands)
  let inputs = permutations [5..9]
  print $ maximum (map (runAmps memory) inputs)
