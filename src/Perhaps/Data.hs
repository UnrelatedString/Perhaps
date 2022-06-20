{-
 - SPDX-FileCopyrightText: 2020-2022 UnrelatedString <https://github.com/UnrelatedString> and other Perhaps contributors
 -
 - SPDX-License-Identifier: BSD-3-Clause
 -}

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE BlockArguments #-}

module Perhaps.Data
    ( Token (CellT, OperatorT),
      Value (Number, Char, List),
      FirstPassCell (FullFunction, PartialFunction),
      hole,
      Cell (Cell, Variad),
      nilad,
      monad,
      dyad,
      contextualize,
      Adicity (Niladic, Monadic, Dyadic),
      Operator (Operator),
      operatorIsUnary,
      derive,
      Number,
      integerMaybe,
      PerhapsFunction,
      Arguments (Arguments),
      left,
      right,
      original,
      ontoLeft,
      ontoRight,
      ontoOriginal,
      onLeft,
      onRight,
      onOriginal,
      swap,
      fill,
      fromLeftRight
    ) where

import Data.Ratio (Rational, numerator, denominator)
--import Control.Category
--import Data.Complex (Complex, realPart, imagPart)

-- Syntactic adicity, not semantic adicity
data Adicity = Niladic | Monadic | Dyadic deriving Show

data Cell = Cell Adicity PerhapsFunction 
          | Variad (Adicity -> PerhapsFunction)

contextualize :: Adicity -> Cell -> (Adicity, PerhapsFunction)
contextualize _ (Cell adicity x) = (adicity, x)
contextualize adicity (Variad f) = (adicity, f adicity)

nilad :: Value -> Cell
nilad = Cell Niladic . const

monad :: (Value -> Value) -> Cell
monad = Cell Monadic . (.left)

dyad :: (Value -> Value -> Value) -> Cell
dyad f = Cell Dyadic \Arguments{left=l, right=r} -> f l r

data Token = CellT Cell
           | OperatorT Operator

data FirstPassCell = FullFunction Cell
                   | PartialFunction (Cell -> Cell)

hole :: FirstPassCell
hole = PartialFunction id

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

-- cyclic imports are illegal :(

-- choice and side effects Later
type PerhapsFunction = Arguments -> Value

data Arguments = Arguments {
    left :: Value,
    right :: Value,
    original :: Value
}

ontoLeft :: Arguments -> Value -> Arguments
ontoLeft a v = a { left = v }

ontoRight :: Arguments -> Value -> Arguments
ontoRight a v = a { right = v }

ontoOriginal :: Arguments -> Value -> Arguments
ontoOriginal a v = a { original = v }

onLeft :: PerhapsFunction -> Arguments -> Arguments
onLeft = (<*>) ontoLeft

onRight :: PerhapsFunction -> Arguments -> Arguments
onRight = (<*>) ontoRight

onOriginal :: PerhapsFunction -> Arguments -> Arguments
onOriginal = (<*>) ontoOriginal

swap :: Arguments -> Arguments
swap a = a { left = right a, right = left a }

fill :: Value -> Arguments
fill v = Arguments v v v

fromLeftRight :: Value -> Value -> Arguments
fromLeftRight l r = Arguments {
    left = l,
    right = r,
    original = l
}
