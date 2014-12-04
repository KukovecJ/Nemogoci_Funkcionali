module Data_Types where

data Bit = Zero | One deriving (Eq,Show)

-- unsigned int tipi kot Word niso poljubno veliki
type Natural = Integer 
type Cantor = Natural -> Bit

(#) :: Bit -> Cantor -> Cantor
-- | Returns the representation of the Bit sequence obtained by
-- prepending x to the Bit sequence represented by a
x # a = \i -> if i == 0 then x else a(i-1)