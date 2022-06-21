{-
 - SPDX-FileCopyrightText: 2020-2022 UnrelatedString <https://github.com/UnrelatedString> and other Perhaps contributors
 -
 - SPDX-License-Identifier: BSD-3-Clause
 -}

module Perhaps.Invoke
    (
    ) where

import System.Environment (getArgs)
import Perhaps.Data
    ( Arguments (Arguments),
      fill,
      fromLeftRight,
      Value (Char, Number, List),
      Adicity (Niladic, Monadic, Dyadic),
      integerMaybe
    )

evaluationParameters :: [Value] -> (Adicity, Arguments)
evaluationParameters [] = (Niladic, fill (Number 0))
evaluationParameters [x] = (Monadic, fill x)
evaluationParameters (x:y:_) = (Dyadic, fromLeftRight x y)

