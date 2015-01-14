import Data_Types
import Functions
import Data.Bool

-- * Testing
coerce :: Bit -> Natural
coerce Zero = 0
coerce One = 1

f, g, h :: Cantor -> Integer

f a = coerce(a(7 * coerce(a 4) +  4 * (coerce(a 7)) + 4))

g a = coerce(a(coerce(a 4) + 11 * (coerce(a 7))))

h a = if a 7 == Zero
      then if a 4 == Zero then coerce(a  4) else coerce(a 11)
      else if a 4 == One  then coerce(a 15) else coerce(a  8)

proj :: Natural -> (Cantor -> Integer)

proj n = \c -> coerce(c n)

pn :: Natural -> Integer
pn n = proj n (\i -> if mod i 2 == 0 then One else Zero)

predikat:: Cantor -> Bool
predikat c = (c 0 == Zero)

a = find predikat
