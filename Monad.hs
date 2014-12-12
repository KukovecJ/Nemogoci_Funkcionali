module Functions where

import Data_Types


-- | S over some type a (e.g. Cantor) will have exactly one field, which represents
-- one of the find functions as defined in Find_X.
newtype S a = S {find :: (a -> Bool) -> a}

-- e.g. iskalnik = S (find_v)

-- | Existential and universal quantifiers defined for searchable sets.
exists_mon, forall_mon :: S a -> (a -> Bool) -> Bool
exists_mon xs p = p(find xs p)
forall_mon xs p = not(exists_mon xs (\x -> not(p x)))

-- | S will be a monad representing searchable sets, a subset of the power set of a given type (e.g. Cantor).
instance Monad S where
  return = singleton
  xs >>= f = bigUnion(image f xs)


-- | Cannonical mapping of an element into a searchable set. Any predicate tested only on a singelton {x}
-- must always return x since it must return something even if the predicate is not satisfied.
singleton :: a -> S a
singleton x = S (\p -> x)


-- | Modified 'find' which returns Nothing if the predicate is not satisfied for any element.
search :: S a -> (a -> Bool) -> Maybe a
search xs p =
    let
        x = find xs p
    in
      if p x then Just x else Nothing


-- | Set-theoretical definition of 'union' where 'bigUnion' creates the union of a familiy of sets.
union :: S a -> S a -> S a
xs `union` ys = bigUnion (doubleton xs ys)


-- | Searching a doubleton {x, y} for an element that satisfies a given predicate p yields x if p(x)
-- holds and must yield some element of {x,y} otherwise, therefore it returns y if p(x) does not hold.
doubleton :: a -> a -> S a
doubleton x y = S (\p -> if p x then x else y)


-- | Given that a subset of a is searchable, the f-image is also searchable. One can find an element in
-- the f-image satisfying a given predicate q by searching the original set for elements satisfying a
-- predicate testing whether q holds for f(x) where x is from a.
image :: (a -> b) -> S a -> S b
image f xs = S (\q -> f(find xs (\x -> q(f x))))


-- | Since none of the functions defined above are restricted to the Cantor set we implement
--  the search over an abstract union in two stages. First, using an auxiliary predicate,
--  we find a member of a given family of sets, that contains some element satisfying the original predicate
-- (note that this gives us no answer as to which element it is at this point).
-- Then, this set can again be searched for the particular element.
bigUnion :: S(S a) -> S a
bigUnion xss = S (\p -> find(find xss (\xs -> exists_mon xs p)) p)
