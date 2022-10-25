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
      integerMaybe
    )

import Perhaps.Misc
    ( antidiagonals
    )

primitiveLookup :: String -> Cell
primitiveLookup "plus" = dyad add
primitiveLookup "double" = monad double
primitiveLookup "reverse" = monad reversePrimitive
primitiveLookup "prime?" = monad isPrime
primitiveLookup "antidiagonals" = monad antidiagonalsPrimitive

add :: Value -> Value -> Value
add (Number x) (Number y) = Number $ x + y
add _ _ = undefined

double :: Value -> Value
double (Number x) = Number $ x * 2
double _ = undefined

reversePrimitive :: Value -> Value
reversePrimitive (List x) = List $ reverse x
reversePrimitive _ = undefined

isPrime :: Value -> Value
-- just use normal booleans until choice is added -- may as well satisfy the prime testing requirement for qualifying as a programming language
-- shitty trial division since rationals are a placeholder anyways
isPrime (Number x)
    | Just i <- integerMaybe x, i > 2 = Number $ fromIntegral $ fromEnum $ all ((>0).mod i) [2..ceiling $ sqrt $ fromIntegral i]
isPrime (Number 2) = Number 1
isPrime _ = Number 0

antidiagonalsPrimitive :: Value -> Value
antidiagonalsPrimitive (List outer) = List $ map List $ antidiagonals $ ohFuck <$> outer
    where ohFuck (List inner) = inner
          ohFuck _ = []
