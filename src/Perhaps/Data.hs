{-
 - SPDX-FileCopyrightText: 2020-2021 UnrelatedString <https://github.com/UnrelatedString> and other Perhaps contributors
 -
 - SPDX-License-Identifier: BSD-3-Clause
 -}

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Perhaps.Data
    ( Token (CellT, OperatorT),
      Value (Number, Char, List),
      FirstPassCell (FullFunction, PartialFunction),
      hole,
      Cell,
      nilad,
      Adicity,
      Operator (Operator),
      operatorIsUnary,
      derive,
      Number,
      integerMaybe
    ) where

import Data.Ratio (Rational, numerator, denominator)
--import Control.Category
--import Data.Complex (Complex, realPart, imagPart)

data Token = CellT Cell
           | OperatorT Operator

data FirstPassCell = FullFunction Cell
                       | PartialFunction (Cell -> Cell)

hole :: FirstPassCell
hole = PartialFunction id

{-
data GenericCell a b = Cell Adicity (a -> b) -- xd

instance Category GenericCell where
    id = Cell id
    (Cell x) . (Cell y) = x . y

type Cell = GenericCell Value Value
-}
type Cell = String -- since until parsing Works again all functions need to be is visible

nilad :: Value -> Cell
nilad = show

-- Syntactic adicity, not semantic adicity
data Adicity = Niladic | Monadic | Dyadic | Variadic (Adicity -> Adicity)

class Adic a where
    adicity :: a -> Adicity

-- TODO: flagged lists -- just using lists for convenience at moment
-- replace before even implementing choice
-- first class functions Eventually:tm:
data Value = Number Number | Char Char | List [Value] deriving (Show)

-- TODO: replace with symbolic math lmao, don't really want to approximate radicals/pi
type Number = Rational

integerMaybe :: Number -> Maybe Integer
integerMaybe x
    | denominator x == 1 = Just $ numerator x
    | otherwise = Nothing

data Operator = Operator {
    operatorIsUnary :: Bool,
    derive :: ([FirstPassCell] -> (FirstPassCell, [FirstPassCell]))
}
