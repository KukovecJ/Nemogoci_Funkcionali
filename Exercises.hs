import Functions
import Data.Bool
import SearchMonad
import Data.Maybe

type N = Integer
type Baire = N -> N
data T = Fork [(N,T)]

-- Programming exercises.

cantorTree :: T
cantorTree = Fork [(0, cantorTree), (1, cantorTree)]

unboundedBreadthTree :: N -> T
unboundedBreadthTree n = Fork [(i, unboundedBreadthTree(n+1)) | i <- [0..n]]


-- Q2 (a):
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


-- Q2 (b):
(#) :: N -> Baire -> Baire
(n # a)    0  = n
(n # a) i  = a (i-1)

findT'    :: T -> (Baire -> Bool) -> Baire

existsT'  :: T -> (Baire -> Bool) -> Bool
existsT' t p = p(findT' t p)

myMatch :: T -> N -> Maybe T
myMatch (Fork ((i, tree) :  [])) n = if i == n then Just tree else Nothing
myMatch (Fork ((i, tree) : xs)) n = let t = myMatch (Fork ((i, tree) : [])) n in
                                    if isJust t then t else myMatch (Fork xs) n

findBranch :: T -> (N -> Bool)-> N
findBranch (Fork ((i, tree) : [])) p = i
findBranch (Fork ((i, tree) : xs)) p = if p i then i else findBranch (Fork xs) p

findT' originalTree p = x # a
    where x = findBranch originalTree (\x -> existsT' (fromJust $ myMatch originalTree x) (\a -> p (x # a)))
          a = findT' (fromJust $ myMatch originalTree x) (\a -> p(x # a))

-- Examples.
b' = findT' cantorTree pr
b = findT' mt pr


-- Q2 (c):

findT''    :: T -> (Baire -> Bool) -> Baire

existsT'' :: T -> (Baire -> Bool) -> Bool
existsT'' t p = p(findT'' t p)

fork :: N -> Baire -> Baire -> Baire
fork x l r i   | i == 0    = x
               | odd i     = l((i-1) `div` 2)
               | otherwise = r((i-2) `div` 2)

findT'' originalTree p = fork x l r
    where x = findBranch originalTree (\x -> let myTree = (fromJust $ myMatch originalTree x) in
                                             existsT'' myTree (\l -> existsT'' myTree (\r -> p(fork x l r))))
          tx = fromJust $ myMatch originalTree x
          l = findT'' tx (\l -> existsT'' tx (\r -> p(fork x l r)))
          r = findT'' tx (\r -> p(fork x l r))

-- Examples.
threetree =  Fork [(0, threetree), (1, threetree), (2, threetree)]

c = findT'' cantorTree pr
c' = findT'' threetree (\b -> b 12 == 2)
