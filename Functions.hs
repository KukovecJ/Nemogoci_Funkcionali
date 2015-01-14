{-# LANGUAGE FlexibleContexts #-}

{-|
Modul: Functions

Opis:  Modul funkcij za testiranje iskalnih funkcij po Cantorjevih množicah.

Mentorja: asist. dr. Matija Pretnar in prof. dr. Andrej Bauer.

Avtorja: Petra Poklukar, Jure Kukovec

-}

module Functions where

import Data_Types
import Find_X

-- * Functions 'find', 'forall' (universal  quantification) and 'exists' (exsistential quantification)

-- | The function 'exists' evaluates to 'Data_types.One' if a binary sequence satisfying the predicate exists and to 'Data_types.Zero' otherwise.
exists :: (Cantor -> Bool) -> Bool
-- | The function 'forall' evaluates to 'Data_types.One' if every binary sequence satisfies the predicate and to 'Data_types.Zero' otherwise.
forall :: (Cantor -> Bool) -> Bool

-- | Given that 'exists' evaluates to 'Data_types.One', 'find' evaluates to some element satisfying the predicate. Even if it evaluates to 'Data_types.Zero', 'find' will always return an element of the Cantor set. Therefore, elements yielded by 'find' must additionally be tested whether or not they satisfy the predicate.
find :: (Cantor -> Bool) -> Cantor

-- Current choice of 'find', for testing purposes.
find = find_vii


-- | The function 'find_i' determines that if a solution beginning with 'Data_types.Zero' exists, the result must also begin with 'Data_Types.Zero'. Otherwise, it must begin with 'Data_types.One'.
find_i :: (Cantor -> Bool) -> Cantor
find_i p = if exists ( \a -> p ( Zero # a ))
            then Zero # find_i ( \a -> p ( Zero # a ) )
			else One  # find_i ( \a -> p ( One  # a ) )

exists p = p ( find ( \a -> p a ) )
forall p = not ( exists ( \a -> not ( p a ) ) )


-- * Functions used for testing
-- | The function 'search' expands the functionality of 'find' and returns a Just value if the result of 'find', which always yields a value in the Cantor space, satisfies the predicate and Nothing otherwise.
search :: (Cantor -> Bool) -> Maybe Cantor
search p = if exists ( \a -> p a )
		then Just ( find ( \a -> p a ) )
		else Nothing


-- | Tests function equality. Two functions f and g are equal iff the values f(x) and g(x) are equal for all x in the domain.
equal :: Eq y => (Cantor -> y) -> (Cantor -> y) -> Bool
equal f g = forall ( \a -> f a == g a )


-- | The definition of implication.
implies :: Bool -> Bool -> Bool
implies p q = not p || q


-- | The value of 'eq' n a b evaluates to True if the sequences a and b match up until the n-th term and to False otherwise.
eq :: Natural -> Cantor -> Cantor -> Bool
eq 0 a b = True
eq n a b = a (n-1) == b (n-1) && eq (n-1) a b


-- | Given a Boolean sequence p, the function 'least' returns the index of the first term of p that equals True.
least :: (Natural -> Bool) -> Natural
least p = if p 0 then 0 else 1 + least(\n -> p (n + 1))


-- | Assuming f is an Integer-valued function defined on the Cantor space, 'modulus' f returns the smallest natural number n, such that for any two elements of the Cantor space a and b, a i == b i for all i <= n 'implies' f(a) = f(b).
modulus :: (Cantor -> Integer) -> Natural
modulus f = least(\n -> forall(\a -> forall(\b -> eq n a b `implies` (f a == f b))))


-- * Alternative implementations of the functions 'exists' and 'find' given that a 'search' function were to be implemented independently.
-- | The function takes the result produced by the now predefined 'search' and maps any Just value to True and Nothing to False.
exists_search :: (Cantor -> Bool) -> Bool
exists_search p = let x = search (\a -> p a) in
                  case x of
                    Nothing -> False
                    Just _ -> True

-- | The function 'find_search' is an analogy to 'find_i', where 'exists_search' replaces 'exists'.
find_search :: (Cantor -> Bool) -> Cantor
find_search p = if exists_search ( \a -> p ( Zero # a ) )
            then Zero # find_search ( \a -> p ( Zero # a ) )
			else One  # find_search ( \a -> p ( One  # a ) )
