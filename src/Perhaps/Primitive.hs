module Perhaps.Primitive
    ( primitiveLookup
    ) where

import Perhaps.Data
    ( Cell (Cell),
      nilad,
      monad,
      dyad,
      Adicity (Niladic, Monadic, Dyadic),
      Value (Number, Char, List)
    )

primitiveLookup :: String -> Cell
primitiveLookup "plus" = monad add
primitiveLookup "double" = dyad double

