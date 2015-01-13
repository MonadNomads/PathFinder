{-# LANGUAGE ExistentialQuantification #-}



import Data.List



data Node  a = Eq a => Node { label :: a }
data Edge  a = Eq a => Edge a a
data Graph a =         Graph { nodes :: [Node a], edges :: [Edge a] } deriving Show



instance Show a => Show (Node a) where
  show (Node x) = show x

instance Show a => Show (Edge a) where
  show (Edge x y) = show x ++ " -> " ++ show y



node :: Graph a -> a -> Maybe (Node a)
node g x = find (\(Node y) -> y == x) (nodes g)

path :: Graph a -> a -> a -> Maybe [a]
path g from to = undefined



example :: Graph String
example = Graph [Node "Vendome", Node "Opera", Node "Couloir"]
                [Edge "Vendome" "Couloir",
                 Edge "Couloir" "Opera"]

main = print example
