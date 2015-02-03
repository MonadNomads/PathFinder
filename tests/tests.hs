import Data.Maybe
import Control.Monad
import Control.Applicative
import Test.QuickCheck
import PathFinder.Graph as G
import PathFinder.PathFinder

newtype ConnectedGraph    a = CG { cg :: Graph a }
newtype DisconnectedGraph a = DG { dg :: Graph a }

instance Arbitrary a => Arbitrary (ConnectedGraph a) where
  arbitrary = join (foldr build start <$> labels)
    where labels        = arbitrary `suchThat` isBigEnough -- Gen [a]
          build l       = (>>= addToCG l)                  -- a -> Gen (ConnectedGraph a) -> Gen (ConnectedGraph a)
          start         = return (CG G.empty)              -- Gen (ConnectedGraph a)
          isBigEnough x = length x > 2                     -- [a] -> Bool

addToCG :: a -> ConnectedGraph a -> Gen (ConnectedGraph a)
addToCG newLabel connectedGraph
  | null (nodes graph) = return singleton
  | otherwise          = fmap buildGraph oldLabelGen
  where graph        = cg connectedGraph                               -- Graph a
        singleton    = CG $ Graph [Node newLabel] []                   -- ConnectedGraph a
        oldLabelGen  = G.label <$> elements (nodes graph)              -- Gen a
        newNodes     = Node newLabel : nodes graph                     -- [Node a]
        newEdges l   = Edge l newLabel : Edge newLabel l : edges graph -- a -> [Edge a]
        buildGraph l = CG $ Graph newNodes (newEdges l)                -- a -> ConnectedGraph a

nodesWorks :: [String] -> String -> Bool
nodesWorks ls l = (l `elem` ls) == isJust (node g l)
  where ns = fmap Node ls
        g = Graph ns []

test = quickCheck . verbose
main = test nodesWorks
