{-# LANGUAGE ExistentialQuantification #-}



import Data.List as L hiding (insert)
import Data.Map  as M hiding ((\\))



data Node  a = Ord a => Node { label :: a }
data Edge  a = Ord a => Edge a a
data Graph a =          Graph { nodes :: [Node a], edges :: [Edge a] } deriving Show



instance Show a => Show (Node a) where
  show (Node x) = show x

instance Show a => Show (Edge a) where
  show (Edge x y) = show x ++ " -> " ++ show y



node :: Graph a -> a -> Maybe (Node a)
node g x = find (\(Node y) -> y == x) (nodes g)

path :: Ord a => Graph a -> a -> a -> Maybe [a]
path g from to = path' g from to (singleton to (0, Nothing))

path' g f t m
  | f `member` m = Just $ build_path m f
  | otherwise    = path' g f t (L.foldr (\n -> extend g n) m (keys m))
                   -- will recurse indefinitely if there is no path

build_path :: Ord a => Map a (Int, Maybe a) -> a -> [a]
build_path m n = let (_, mp) = m ! n in
                  case mp of
                   Nothing -> [n]
                   Just p  -> n : build_path m p

extend :: Ord a => Graph a -> a -> Map a (Int, Maybe a) -> Map a (Int, Maybe a)
extend g n m =
  let (l, _) = m ! n
      ps = precs g n
      ns = ps \\ (keys m) in
  L.foldr (\p -> insert p (l + 1, Just n)) m ns

precs :: Graph a -> a -> [a]
precs g n = [f | Edge f t <- (edges g), t == n]



example :: Graph String
example = Graph [Node "Vendome", Node "Opera", Node "Couloir"]
                [Edge "Vendome" "Couloir",
                 Edge "Couloir" "Opera"]

main = print $ path example "Vendome" "Opera"
