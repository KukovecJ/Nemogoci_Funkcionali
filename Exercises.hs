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

shiftPredicate :: (Baire->Bool) -> (Baire -> Bool)
shiftPredicate p i = \sequence -> p (\n -> sequence (n + i))

findTaux (Fork [s]) p 0 = let (i, tree) = s in i
findTaux (Fork [s]) p n = let (i, tree) = s in findTaux tree (shiftPredicate p 1) (n-1)
findTaux (Fork x:xs) p n = let (i, tree) = x in
    if p (\i -> findTaux (Fork [x]) p i )  then i
       else findTaux (Fork xs) p n

findT = findTaux

predicate :: ( Baire -> Bool)
predicate b = b 0 == 0
