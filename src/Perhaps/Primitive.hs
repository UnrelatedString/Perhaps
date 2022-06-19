module Perhaps.Primitive
    ( primitiveLookup
    ) where

import Perhaps.Data
    ( Cell
    )

primitiveLookup :: String -> Cell
primitiveLookup = id --lmao
