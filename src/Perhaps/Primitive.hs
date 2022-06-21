{-
 - SPDX-FileCopyrightText: 2020-2022 UnrelatedString <https://github.com/UnrelatedString> and other Perhaps contributors
 -
 - SPDX-License-Identifier: BSD-3-Clause
 -}

module Perhaps.Primitive
    ( primitiveLookup,
      reversePrimitive -- am starting to think maybe I should move to blanket imports for internal modules :P
    ) where

import Perhaps.Data
    ( Cell (Cell),
      nilad,
      monad,
      dyad,
      Adicity (Niladic, Monadic, Dyadic),
      Value (Number, Char, List),
    )

primitiveLookup :: String -> Cell
primitiveLookup "plus" = dyad add
primitiveLookup "double" = monad double
primitiveLookup "reverse" = monad reversePrimitive

add :: Value -> Value -> Value
add (Number x) (Number y) = Number $ x + y
add _ _ = undefined

double :: Value -> Value
double (Number x) = Number $ x * 2
double _ = undefined

reversePrimitive :: Value -> Value
reversePrimitive (List x) = List $ reverse x
reversePrimitive _ = undefined