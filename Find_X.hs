module Find_X where

import Data_Types

-- | A mild improvement over "find_i", with no calls to "exists".
find_ii :: (Cantor -> Bool) -> Cantor
find_ii p = if p(Zero # find_ii(\a -> p(Zero # a)))
             then Zero # find_ii(\a -> p(Zero # a))
             else One  # find_ii(\a -> p(One  # a))

-- | Saves the result yielded by the evaluation of the given predicate, to be used when needed.
find_iii :: (Cantor -> Bool) -> Cantor
find_iii p = h # find_iii(\a -> p(h # a))
			where h = if p(Zero # find_iii(\a -> p(Zero # a))) then Zero else One


-- | An improvement over "find_i", but worse that "find_iii", due to not having pruned its branching via laziness.
find_iv :: (Cantor -> Bool) -> Cantor
find_iv p = let leftbranch = Zero # find_iv(\a -> p(Zero # a))
            in if p(leftbranch)
               then leftbranch
               else One # find_iv(\a -> p(One # a))

-- | Calculates the minimal number of positions required, to yield the desired position. Starting from the requested position it calculates
-- prior positions if needed and constructs the tail of the solution recursively when necessary.
find_v :: (cantor -> bool) -> cantor
find_v p = \n ->  if q n (find_v(q n)) then Zero else One
    where q n a = p(\i -> if i < n then find_v p i else if i == n then Zero else a(i-n-1))


-- | Not implemented, due to the author's concerns about efficiency.
find_vi :: (Cantor -> Bool) -> Cantor
--find_vi p = b
--    where b = \n -> if q n (find_vi(q n)) then zero else one
--              q n a = p(\i -> if i < n then b i else if i == n then zero else a(i-n-1))


-- | Improved 'find_v' with memoisation using the tree datatype.
find_vii :: (Cantor -> Bool) -> Cantor
find_vii p = b
    where b = id'(\n -> if q n (find_vii(q n)) then Zero else One)
          q n a = p(\i -> if i < n then b i else if i == n then Zero else a(i-n-1))

-- | Tree datatype.
data T x = B x (T x) (T x)


-- | Sequence-to-tree isomorphism.
code :: (Natural -> x) -> T x
code f = B (f 0) (code(\n -> f(2*n+1))) (code(\n -> f(2*n+2)))

-- |  Inverse of 'code'.
decode :: T x -> (Natural -> x)
decode (B x l r) n |  n == 0    = x
                   |  odd n     = decode l ((n-1) `div` 2)
                   |  otherwise = decode r ((n-2) `div` 2)

-- | Identity composition.
id' :: (Natural -> x) -> (Natural -> x)
id' = decode.code
