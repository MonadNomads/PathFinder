{-# LANGUAGE TemplateHaskell #-}


import System.Exit
import Data.List
import Data.Maybe
import Control.Monad
import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Test
import PathFinder.Graph as G
import PathFinder.PathFinder



instance (Arbitrary a, Eq a) => Arbitrary (Graph a) where
  arbitrary = do
    nodes <- nub <$> listOf1 arbitrary
    esize <- arbitrary `suchThat` (< 2 * length nodes)
    edges <- (nub . concat) <$> (vectorOf esize
                                 (do f <- elements nodes
                                     t <- elements nodes `suchThat` (/= f)
                                     return [(f, t), (t, f)]))
    return $ Graph (map Node nodes) (map (uncurry Edge) edges)



prop_nodesWorks :: Eq a => [a] -> a -> Bool
prop_nodesWorks ls l = (l `elem` ls) == isJust (node g l)
  where ns = fmap Node ls
        g = Graph ns []

prop_connectedPaths :: Ord a => Graph a -> Bool
prop_connectedPaths g = isConnected g == all isJust [path g x y | (Node x) <- nodes g, (Node y) <- nodes g]



main = do
  result <- $quickCheckAll
  unless result exitFailure
