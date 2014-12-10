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
		
-- | To be explained later.	Some recursion/laziness magic.	
find_v :: (Cantor -> Bool) -> Cantor
find_v p = \n ->  if q n (find_v(q n)) then Zero else One
		where q n a = p(\i -> if i < n then find_v p i else if i == n then Zero else a(i-n-1))

-- | Desc.		
find_vi :: (Cantor -> Bool) -> Cantor
find_vi p = b
		where b = \n -> if q n (find_vi(q n)) then Zero else One
        q n a = p(\i -> if i < n then b i else if i == n then Zero else a(i-n-1))


find_vii :: (Cantor -> Bool) -> Cantor
