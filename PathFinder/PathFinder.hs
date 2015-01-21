module PathFinder.PathFinder (path) where

import Data.List hiding (insert)
import Data.Map hiding ((\\), foldr)

import PathFinder.Graph

type VisitedNodes a = Map a (Int, Maybe a)

path :: Ord a => Graph a -> a -> a -> Maybe [a]
path g from to = path' g from to (singleton to (0, Nothing))

path' g f t m
  | f `member` m = Just $ build_path m f
  | otherwise    = let m' = foldr (\n -> extend g n) m (keys m) in
                    if m' == m then Nothing else path' g f t m'

build_path :: Ord a => VisitedNodes a -> a -> [a]
build_path m n = let (_, mp) = m ! n in
                  case mp of
                   Nothing -> [n]
                   Just p  -> n : build_path m p

extend :: Ord a => Graph a -> a -> VisitedNodes a -> VisitedNodes a
extend g n m =
  let (l, _) = m ! n
      ps = parents g n
      ns = ps \\ (keys m) in
  foldr (\p -> insert p (l + 1, Just n)) m ns

