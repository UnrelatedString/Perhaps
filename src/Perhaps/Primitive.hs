module Perhaps.Primitive
    ( primitiveLookup
    ) where

import Perhaps.Data
    ( Cell (Cell),
      Adicity (Niladic, Monadic, Dyadic)
    )

primitiveLookup :: String -> Cell
primitiveLookup x = Cell (cycle [Dyadic, Monadic] !! fromEnum (head x)) x