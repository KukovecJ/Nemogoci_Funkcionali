module Functions where

import Data_Types

-- | The functions "exists" and "forall" test a given logical predicate
-- on the Cantor set. The function "exists" evaluates to
-- One if a binary sequence satisfying the predicate exists and to Zero otherwise.
-- The function "forall" evaluates to One if every binary sequence satisfies the predicate
-- and to Zero otherwise.
exists, forall :: (Cantor -> Bool) -> Bool

-- | Given that "exists" evaluates to One,
-- "find" evaluates to some sequence satisfying the predicate.
find :: (Cantor -> Bool) -> Cantor

find = find_i

exists p = p ( find ( \a -> p a ) )
forall p = not ( exists ( \a -> not ( p a ) ) )

find_i :: (Cantor -> Bool) -> Cantor
find_i p = if exists ( \a -> p ( Zero # a ) )
            then Zero # find_i ( \a -> p ( Zero # a ) )
			else One  # find_i ( \a -> p ( One  # a ) )

-- | The function "search" expands the functionality of "find" and returns a Just value if the result of "find", which always yields a value in the Cantor space, satisfies the predicate and Nothing otherwise.
search :: (Cantor -> Bool) -> Maybe Cantor
search p = if exists ( \a -> p a )
		then Just ( find ( \a -> p a ) )
		else Nothing

-- | Tests function equality. Two functions are equal iff for all x in the domain the values f(x) and g(x) are equal.
equal :: Eq y => (Cantor -> y) -> (Cantor -> y) -> Bool
equal f g = forall ( \a -> f a == g a )

-- | Alternative implementations of the functions "exists" and "find" given that a "search" function were to be implemented independently.
exists_search :: (Cantor -> Bool) -> Bool
exists_search p = let x = search (\a -> p a) in
                  case x of
                    Nothing -> False
                    Just _ -> True


find_search :: (Cantor -> Bool) -> Cantor
find_search p = if exists_search ( \a -> p ( Zero # a ) )
            then Zero # find_search ( \a -> p ( Zero # a ) )
			else One  # find_search ( \a -> p ( One  # a ) )


-- | Definition of implication.
implies :: Bool -> Bool -> Bool
implies p q = not p || q

-- | The value of eq n a b evaluates to True if the sequences a and b match up until the n-th term and to False otherwise.
eq :: Natural -> Cantor -> Cantor -> Bool
eq 0 a b = True
eq n a b = a (n-1) == b (n-1) && eq (n-1) a b

-- | Given a boolean sequence p, the function "least" returns the index of the first term of p that equals True.
least :: (Natural -> Bool) -> Natural
least p = if p 0 then 0 else 1 + least(\n -> p (n + 1))

-- | Assuming f is a function defined on the Cantor space, "modulus" f returns the smallest natural number n such that for any two elements of the Cantor space a and b, f(a) = f(b) is implied by a i = b i for all i <= n.
modulus :: (Cantor -> Integer) -> Natural
modulus f = least(\n -> forall(\a -> forall(\b -> eq n a b `implies` (f a == f b))))
