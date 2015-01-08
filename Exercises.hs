import Data_Types
import Functions
import Data.Bool
import SearchMonad


type N = Integer
type Baire = N -> N
data T = Fork [(N,T)]


cantorTree :: T
cantorTree = Fork [(0, cantorTree), (1, cantorTree)]

unboundedBreadthTree :: N -> T
unboundedBreadthTree n = Fork [(i, unboundedBreadthTree(n+1)) | i <- [0..n]]


-- Naloga 1:
findT   :: T -> (Baire -> Bool) -> Baire
existsT :: T -> (Baire -> Bool) -> Bool

existsT t p = p(findT t p)

shiftPredicate :: (Baire->Bool)-> N -> (Baire -> Bool)
shiftPredicate p x = \sequence -> p (\n -> if n == 0 then x else sequence (n-1))


findTaux (Fork ((i, tree) : [])) p = \n -> if n == 0 then i else findTaux tree (shiftPredicate p i) (n-1)
findTaux (Fork ((i, tree) : xs)) p = let solution = findTaux (Fork ((i, tree) : [])) p
                                     in \n -> if p solution then solution n
                                              else findTaux (Fork xs) p n

findT = findTaux

pr :: ( Baire -> Bool)
pr b = b 12 == 1

myTree :: N ->  T
myTree n = Fork [(n,myTree (n+1))]
mt = myTree 0

-- Example.
a = findT cantorTree pr

-- Naloga 2:
findT'    :: T -> (Baire -> Bool) -> Baire
forsomeT' :: T -> (Baire -> Bool) -> Bool

forsomeT' t p = p(findT' t p)
