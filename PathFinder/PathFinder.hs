module PathFinder.PathFinder (path) where

import Data.List hiding (insert)
import Data.Map hiding ((\\), foldr)

import PathFinder.Graph

type VisitedNodes a = Map a (Int, Maybe a)

path :: Ord a => Graph a -> a -> a -> Maybe [a]
path g from to = path' g from to (singleton to (0, Nothing))

path' g f t m
  | f `member` m = Just $ buildPath m f
  | otherwise    = let m' = foldr (extend g) m (keys m) in
                    if m' == m then Nothing else path' g f t m'

buildPath :: Ord a => VisitedNodes a -> a -> [a]
buildPath m n = let (_, mp) = m ! n in
                  case mp of
                   Nothing -> [n]
                   Just p  -> n : buildPath m p

extend :: Ord a => Graph a -> a -> VisitedNodes a -> VisitedNodes a
extend g n m =
  let (l, _) = m ! n
      ps = parents g n
      ns = ps \\ keys m in
  foldr (\p -> insert p (l + 1, Just n)) m ns

