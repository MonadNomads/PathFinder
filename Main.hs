import PathFinder.Graph
import PathFinder.PathFinder

example :: Graph String
example = Graph [Node "Vendome", Node "Opera", Node "Couloir"]
                [Edge "Vendome" "Couloir" 1,
                 Edge "Couloir" "Opera"   2]

main = print $ path example "Vendome" "Opera"
