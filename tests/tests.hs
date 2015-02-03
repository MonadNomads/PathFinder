import PathFinder.Graph as G
import PathFinder.PathFinder
import Test.QuickCheck
import Data.Maybe

nodesWorks :: [String] -> String -> Bool
nodesWorks ls l = (l `elem` ls) == isJust (node g l)
  where ns = fmap Node ls
        g = Graph ns []

test = quickCheck . verbose
main = test nodesWorks
