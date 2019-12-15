module IntcodeComputer (StdIO, cin, cout, Memory, preprocess, waitForExit, loadProgram) where

import Control.Concurrent
import Control.Concurrent.Chan
import Data.Map (Map, insert, findWithDefault, (!), fromList, empty)
import Helpers (split, readInteger)

type IP = Integer
type Instruction = Integer
type Mode = Integer
type Memory = Map Integer Integer
type Channel = Chan (Maybe Integer)
data StdIO = StdIO {cin :: Channel, cout :: Channel}
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

runAdd :: IP -> Memory -> RelativeBase -> StdIO -> ParamMode -> IO ()
runAdd ip memory base stdio modes = run (ip + 4) newMemory base stdio
  where value1 = readParameter ip 1 memory base modes
        value2 = readParameter ip 2 memory base modes
        addr3 = readAddress ip 3 memory base modes
        value3 = value1 + value2
        newMemory = insert addr3 value3 memory

runMult :: IP -> Memory -> RelativeBase -> StdIO -> ParamMode -> IO ()
runMult ip memory base stdio modes = run (ip + 4) newMemory base stdio
  where value1 = readParameter ip 1 memory base modes
        value2 = readParameter ip 2 memory base modes
        addr3 = readAddress ip 3 memory base modes
        value3 = value1 * value2
        newMemory = insert addr3 value3 memory

runRead :: IP -> Memory -> RelativeBase -> StdIO -> ParamMode -> IO ()
runRead ip memory base stdio modes = do
                                  let addr1 = readAddress ip 1 memory base modes
                                  Just value <- readChan (cin stdio)
                                  let newMemory = insert addr1 value memory
                                  run (ip + 2) newMemory base stdio

runPrint :: IP -> Memory -> RelativeBase -> StdIO -> ParamMode -> IO ()
runPrint ip memory base stdio modes = do
                                  let value = readParameter ip 1 memory base modes
                                  writeChan (cout stdio) (Just value)
                                  run (ip + 2) memory base stdio 

runJumpIfTrue :: IP -> Memory -> RelativeBase -> StdIO -> ParamMode -> IO ()
runJumpIfTrue ip memory base stdio modes
  | condition == True = run value2 memory base stdio
  | otherwise = run (ip + 3) memory base stdio
  where value1 = readParameter ip 1 memory base modes
        value2 = readParameter ip 2 memory base modes
        condition = value1 /= 0

runJumpIfFalse :: IP -> Memory -> RelativeBase -> StdIO -> ParamMode -> IO ()
runJumpIfFalse ip memory base stdio modes
  | condition == True = run value2 memory base stdio
  | otherwise = run (ip + 3) memory base stdio
  where value1 = readParameter ip 1 memory base modes
        value2 = readParameter ip 2 memory base modes
        condition = value1 == 0

runLessThan :: IP -> Memory -> RelativeBase -> StdIO -> ParamMode -> IO ()
runLessThan ip memory base stdio modes = run (ip + 4) newMemory base stdio
  where value1 = readParameter ip 1 memory base modes
        value2 = readParameter ip 2 memory base modes
        addr3 = readAddress ip 3 memory base modes
        result = if value1 < value2 then 1 else 0
        newMemory = insert addr3 result memory

runEquals :: IP -> Memory -> RelativeBase -> StdIO -> ParamMode -> IO ()
runEquals ip memory base stdio modes = run (ip + 4) newMemory base stdio
  where value1 = readParameter ip 1 memory base modes
        value2 = readParameter ip 2 memory base modes
        addr3 = readAddress ip 3 memory base modes
        result = if value1 == value2 then 1 else 0
        newMemory = insert addr3 result memory

runRelBase :: IP -> Memory -> RelativeBase -> StdIO -> ParamMode -> IO ()
runRelBase ip memory base stdio modes = run (ip + 2) memory newBase stdio
  where value1 = readParameter ip 1 memory base modes
        newBase = base + value1

runExit :: IP -> Memory -> RelativeBase -> StdIO -> ParamMode -> IO ()
runExit ip memory base stdio modes = writeChan (cout stdio) Nothing

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

run :: IP -> Memory -> RelativeBase -> StdIO -> IO ()
run ip memory base stdio
  | ins == 1 = runAdd ip memory base stdio modes
  | ins == 2 = runMult ip memory base stdio modes
  | ins == 3 = runRead ip memory base stdio modes
  | ins == 4 = runPrint ip memory base stdio modes
  | ins == 5 = runJumpIfTrue ip memory base stdio modes
  | ins == 6 = runJumpIfFalse ip memory base stdio modes
  | ins == 7 = runLessThan ip memory base stdio modes
  | ins == 8 = runEquals ip memory base stdio modes
  | ins == 9 = runRelBase ip memory base stdio modes
  | ins == 99 = runExit ip memory base stdio modes
    where op = memory ! ip
          (ins, modes) = parseModes op

preprocess :: Memory -> IO (StdIO)
preprocess memory = do
  input <- newChan
  output <- newChan
  let stdio = StdIO input output
  forkIO (run 0 memory 0 stdio)
  return stdio

waitForExit :: StdIO -> IO [Integer]
waitForExit stdio = do
  writeChan (cin stdio) Nothing
  out <- internalWaitForExit (Just 0) (cout stdio)
  return (tail out) -- Remove Just 0 we started with

internalWaitForExit :: Maybe Integer -> Channel -> IO [Integer]
internalWaitForExit (Just v) c = do
            r <- readChan c
            z <- internalWaitForExit r c
            return (v:z)
internalWaitForExit Nothing c = return []

loadProgram :: IO Memory
loadProgram = do
  commands <- fmap (map readInteger) (fmap (split ',') getLine)
  return $ fromList (zip [0..] commands)

