import Data_Types
import qualified Functions as F
import Data.Bool

coerce :: Bit -> Natural
coerce Zero = 0
coerce One = 1

f, g, h :: Cantor -> Integer

f a = coerce(a(7 * coerce(a 4) +  4 * (coerce(a 7)) + 4))

g a = coerce(a(coerce(a 4) + 11 * (coerce(a 7))))

h a = if a 7 == Zero
      then if a 4 == Zero then coerce(a  4) else coerce(a 11)
      else if a 4 == One  then coerce(a 15) else coerce(a  8)
	  
