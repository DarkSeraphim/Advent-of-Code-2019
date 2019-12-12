import Helpers (split)
import qualified Data.Map as M (empty, Map, (!), lookup, insert)

data Tree = Empty | Node String (Maybe String) [Tree]

unpack :: String -> Maybe Tree -> Tree
unpack id (Just node) = node
unpack id Nothing = Node id Nothing []

getNode :: String -> M.Map String Tree -> Tree
getNode id map = node
  where maybeNode = M.lookup id map
        node = unpack id maybeNode

setParent :: Tree -> Tree -> Tree
setParent (Node id _ children) (Node pid _ _) = Node id (Just pid) children

addChild :: Tree -> Tree -> Tree
addChild (Node id pp children) child = Node id pp (child:children)

updateTree :: String -> String -> M.Map String Tree -> M.Map String Tree
updateTree parent child map = newMap
  where p = getNode parent map
        c = getNode child map
        nc = setParent c p
        np = addChild p nc
        newMap = M.insert parent np (M.insert child nc map)

getRoot :: Tree -> M.Map String Tree -> Tree
getRoot (Node id Nothing _) map = map M.! id
getRoot (Node _ (Just parent) _) map = getRoot (map M.! parent) map

getOrbits :: Integer -> Tree -> M.Map String Tree -> [(String, Integer)]
getOrbits d Empty map = []
getOrbits d (Node id Nothing _) map = [(id, d)]
getOrbits d (Node id (Just parent) _) map = (id, d) : result
  where result = getOrbits (d + 1) (map M.! parent) map
        (_, highest) = head result

isSame :: (String, Integer) -> (String, Integer) -> Bool
isSame (idA, _) (idB, _) = idA == idB

getDist :: (String, Integer) -> Integer
getDist (_, d) = d

main = do
  edges <- fmap (map (split ')')) (fmap lines getContents)
  let nodes = foldr (\(p:c:_) map -> updateTree p c map) M.empty edges
  let you = getOrbits 0 (nodes M.! "YOU") nodes
  let san = getOrbits 0 (nodes M.! "SAN") nodes
  let m = minimum [(getDist y) + (getDist s) | y <- you, s <- san, isSame y s]
  print (m - 2) -- Starting nodes are included, so exclude them from the distance
