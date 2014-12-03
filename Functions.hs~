module Functions where

import Data_Types

exists, forall :: (Cantor -> Bool) -> Bool
-- | The functions "exists" and "forall" test a given logical predicate
-- on the Cantor set. The function "exists" evaluates to 
-- One if a binary sequence satisfying the predicate exists and to Zero otherwise.
-- The function "forall" evaluates to One if every binary sequence satisfies the predicate 
-- and to Zero otherwise.
find :: (Cantor -> Bool) -> Cantor
-- | Given that "exists" evaluates to One, 
-- "find" evaluates to some sequence satisfying the predicate.

find = find_i

exists p = p ( find ( \a -> p a ) )
forall p = not ( exists ( \a -> not ( p a ) ) )

find_i :: (Cantor -> Bool) -> Cantor
find_i p = if exists ( \a -> p ( Zero # a ) )
            then Zero # find_i ( \a -> p ( Zero # a ) )
			else One  # find_i ( \a -> p ( One  # a ) )

search :: (Cantor -> Bool) -> Maybe Cantor
-- | Search
search p = if exists ( \a -> p a )
		then Just ( find ( \a -> p a ) )
		else Nothing
			
equal :: Eq y => (Cantor -> y) -> (Cantor -> y) -> Bool
equal f g = forall ( \a -> f a == g a )
