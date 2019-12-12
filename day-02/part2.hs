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

preprocess :: Memory -> Integer -> Integer -> Integer
preprocess memory noun verb =
  -- Manually keep inserting numbers till it works
  run 0 (insert 1 noun (insert 2 verb memory))

answer :: [Integer] -> Integer
answer (_:noun:verb:_) = noun * 100 + verb

main = do
  commands <- fmap (map readInteger) (fmap (split ',') getLine)
  let memory = fromList (zip [0..] commands)
  let outputs = [[(preprocess memory noun verb), noun, verb] | noun <- [0..99], verb <- [0..99]]
  print $ answer $ head $ filter (\(output:noun:verb) -> output == 19690720) outputs
