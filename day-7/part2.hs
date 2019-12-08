import Control.Concurrent
import Control.Concurrent.Chan
import Data.List (permutations)
import Data.Map (Map, insert, (!), fromList)
import Helpers (split, readInteger)

type IP = Integer
type Instruction = Integer
type Mode = Integer
type Memory = Map Integer Integer
type Input = [Chan (Maybe Integer)]
type ParamMode = [Mode]

readParameter :: IP -> Integer -> Memory -> ParamMode -> Integer
readParameter ip i memory modes 
  | mode == 0 = memory ! value -- address mode
  | mode == 1 = value          -- immediate mode
    where mode = modes !! (fromIntegral i)
          value = memory ! (ip + i)

runAdd :: IP -> Memory -> Input -> ParamMode -> IO ()
runAdd ip memory input modes = run (ip + 4) newMemory input
  where value1 = readParameter ip 1 memory modes
        value2 = readParameter ip 2 memory modes
        addr3 = memory ! (ip + 3)
        value3 = value1 + value2
        newMemory = insert addr3 value3 memory

runMult :: IP -> Memory -> Input -> ParamMode -> IO ()
runMult ip memory input modes = run (ip + 4) newMemory input
  where value1 = readParameter ip 1 memory modes
        value2 = readParameter ip 2 memory modes
        addr3 = memory ! (ip + 3)
        value3 = value1 * value2
        newMemory = insert addr3 value3 memory

runRead :: IP -> Memory -> Input -> ParamMode -> IO ()
runRead ip memory input modes = do
                                  let addr1 = memory ! (ip + 1)
                                  let cin = input !! 0
                                  Just value <- readChan cin
                                  let newMemory = insert addr1 value memory
                                  run (ip + 2) newMemory input

runPrint :: IP -> Memory -> Input -> ParamMode -> IO ()
runPrint ip memory input modes = do
                                  let value = readParameter ip 1 memory modes
                                  let cout = input !! 1
                                  writeChan cout (Just value)
                                  run (ip + 2) memory input 

runJumpIfTrue :: IP -> Memory -> Input -> ParamMode -> IO ()
runJumpIfTrue ip memory input modes
  | condition == True = run value2 memory input
  | otherwise = run (ip + 3) memory input
  where value1 = readParameter ip 1 memory modes
        value2 = readParameter ip 2 memory modes
        condition = value1 /= 0

runJumpIfFalse :: IP -> Memory -> Input -> ParamMode -> IO ()
runJumpIfFalse ip memory input modes
  | condition == True = run value2 memory input
  | otherwise = run (ip + 3) memory input
  where value1 = readParameter ip 1 memory modes
        value2 = readParameter ip 2 memory modes
        condition = value1 == 0

runLessThan :: IP -> Memory -> Input -> ParamMode -> IO ()
runLessThan ip memory input modes = run (ip + 4) newMemory input
  where value1 = readParameter ip 1 memory modes
        value2 = readParameter ip 2 memory modes
        addr3 = memory ! (ip + 3)
        result = if value1 < value2 then 1 else 0
        newMemory = insert addr3 result memory

runEquals :: IP -> Memory -> Input -> ParamMode -> IO ()
runEquals ip memory input modes = run (ip + 4) newMemory input
  where value1 = readParameter ip 1 memory modes
        value2 = readParameter ip 2 memory modes
        addr3 = memory ! (ip + 3)
        result = if value1 == value2 then 1 else 0
        newMemory = insert addr3 result memory

runExit :: IP -> Memory -> Input -> ParamMode -> IO ()
runExit ip memory input modes = writeChan (input !! 1) Nothing

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

run :: IP -> Memory -> Input -> IO ()
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

preprocess :: Memory -> [Integer] -> IO Integer
preprocess memory phases = do
  chanA <- newChan
  chanB <- newChan
  chanC <- newChan
  chanD <- newChan
  chanE <- newChan
  output <- dupChan chanA
  forkIO (run 0 memory [chanA, chanB])
  forkIO (run 0 memory [chanB, chanC])
  forkIO (run 0 memory [chanC, chanD])
  forkIO (run 0 memory [chanD, chanE])
  forkIO (run 0 memory [chanE, chanA])
  writeChan chanA (Just (phases !! 0))
  writeChan chanB (Just (phases !! 1))
  writeChan chanC (Just (phases !! 2))
  writeChan chanD (Just (phases !! 3))
  writeChan chanE (Just (phases !! 4))
  writeChan chanA (Just (toInteger 0))
  results <- waitForExit (Just (toInteger 0)) output
  return (last results)

unpack :: Maybe a -> a -> a
unpack (Just a) b = a
unpack Nothing b = b

waitForExit :: Maybe a -> Chan (Maybe a) -> IO [a]
waitForExit (Just v) c = do
            r <- readChan c
            z <- waitForExit r c
            return (v:z)
waitForExit Nothing c = return []

main = do
  commands <- fmap (map readInteger) (fmap (split ',') getLine)
  let memory = fromList (zip [0..] commands)
  let inputs = permutations [5..9]
  results <- sequence (map (preprocess memory) inputs)
  print (maximum results)
