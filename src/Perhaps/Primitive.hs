module Perhaps.Primitive
    ( primitiveLookup
    ) where

import Perhaps.Data
    ( PerhapsFunction
    )

primitiveLookup :: String -> PerhapsFunction
primitiveLookup = id --lmao
