{-# LANGUAGE ExistentialQuantification #-}

module PathFinder.Graph where

import Data.List

data Node  a = Ord a => Node { label :: a }
data Edge  a = Ord a => Edge a a
data Graph a =          Graph { nodes :: [Node a], edges :: [Edge a] } deriving Show

instance Show a => Show (Node a) where
  show (Node x) = show x

instance Show a => Show (Edge a) where
  show (Edge x y) = show x ++ " -> " ++ show y

node :: Graph a -> a -> Maybe (Node a)
node g x = find (\(Node y) -> y == x) (nodes g)

parents :: Graph a -> a -> [a]
parents g n = [f | Edge f t <- edges g, t == n]
