data Bit = Zero | One 
	deriving (Eq)

type Natural = Integer
type Cantor = Natural -> Bit

(#) :: Bit -> Cantor -> Cantor
x # a = \i -> if i == 0 then x else a(i-1)