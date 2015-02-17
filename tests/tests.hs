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

prop_triangularInequality :: Ord a => Graph a -> Property
prop_triangularInequality g = length (nodes g) >= 3
                            && isJust pab
                            && isJust pbc ==> maybe False id $ do
                              lac <- length <$> pac
                              lab <- length <$> pab
                              lbc <- length <$> pbc
                              return $ lac <= lab + lbc
  where a:b:c:_ = take 3 (nodes g)
        pab = path g (G.label a) (G.label b)
        pbc = path g (G.label b) (G.label c)
        pac = path g (G.label a) (G.label c)


main = do
  result <- $quickCheckAll
  unless result exitFailure
