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

getOrbits :: Tree -> M.Map String Tree -> Integer
getOrbits Empty map = 0
getOrbits (Node _ Nothing _) map = 0
getOrbits (Node _ (Just parent) _) map = 1 + getOrbits (map M.! parent) map

lengthT :: Tree -> Integer
lengthT Empty = 0
lengthT (Node _ _ trees) = 1 + (foldr (\x y -> x + y) 0 (map lengthT trees))

main = do
  edges <- fmap (map (split ')')) (fmap lines getContents)
  let nodes = foldr (\(p:c:_) map -> updateTree p c map) M.empty edges
  let tree = getRoot (nodes M.! (head (head edges))) nodes
  print $ foldr (\node orbits -> (getOrbits node nodes) + orbits) 0 nodes

