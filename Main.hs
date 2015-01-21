import PathFinder.Graph
import PathFinder.PathFinder

example :: Graph String
example = Graph [Node "Vendome", Node "Opera", Node "Couloir"]
                [Edge "Vendome" "Couloir",
                 Edge "Couloir" "Opera"]

main = print $ path example "Vendome" "Opera"
