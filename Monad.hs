module Functions where

import Data_Types

image :: (a -> b) -> S a -> S b
union :: S a -> S a -> S a
bigUnion :: S(S a) -> S a

-- | S over some type a (e.g. Cantor) will have exactly one field, which represents
-- one of the find functions as defined in Find_X.
newtype S a = S {find :: (a -> Bool) -> a}

-- | S will be a monad representing searchable sets, a subset of the power set of a given type (e.g. Cantor).
instance Monad S where
  return = singleton
  xs >>= f = bigUnion(image f xs)
  
  
-- | Cannonical mapping of an element into a searchable set. Each element can be represented
-- by a function that yields said element regardless of input.
singleton :: a -> S a
singleton x = S (\t -> x)

-- | .
search :: S a -> (a -> Bool) -> Maybe a
search xs p = 
