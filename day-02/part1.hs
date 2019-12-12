import Data.Map (Map, insert, (!), fromList)
import Helpers (split, readInteger)

type IP = Integer
type Memory = Map Integer Integer

runAdd :: IP -> Memory -> Integer
runAdd ip memory = run (ip + 4) newMemory
  where addr1 = memory ! (ip + 1)
        addr2 = memory ! (ip + 2)
        addr3 = memory ! (ip + 3)
        value1 = memory ! addr1
        value2 = memory ! addr2
        value3 = value1 + value2
        newMemory = insert addr3 value3 memory

runMult :: IP -> Memory -> Integer
runMult ip memory = run (ip + 4) newMemory
  where addr1 = memory ! (ip + 1)
        addr2 = memory ! (ip + 2)
        addr3 = memory ! (ip + 3)
        value1 = memory ! addr1
        value2 = memory ! addr2
        value3 = value1 * value2
        newMemory = insert addr3 value3 memory

runExit :: IP -> Memory -> Integer
runExit ip memory = memory ! 0

run :: IP -> Memory -> Integer
run ip memory
  | op == 1 = runAdd ip memory
  | op == 2 = runMult ip memory
  | op == 99 = runExit ip memory
    where
          op = memory ! ip

preprocess :: Memory -> Integer
preprocess memory =
  run 0 (insert 2 2 (insert 1 12 memory))

main = do
  commands <- fmap (map readInteger) (fmap (split ',') getLine)
  print $ preprocess (fromList (zip [0..] commands))
