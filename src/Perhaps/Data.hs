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
      Cell (Cell, Variad),
      PerhapsFunction,
      nilad,
      contextualize,
      Adicity (Niladic, Monadic, Dyadic),
      Operator (Operator),
      operatorIsUnary,
      derive,
      Number,
      integerMaybe
    ) where

import Data.Ratio (Rational, numerator, denominator)
--import Control.Category
--import Data.Complex (Complex, realPart, imagPart)

-- Syntactic adicity, not semantic adicity
data Adicity = Niladic | Monadic | Dyadic deriving Show

{-
data GenericCell a b = Cell Adicity (a -> b) -- xd

instance Category GenericCell where
    id = Cell id
    (Cell x) . (Cell y) = x . y

type Cell = GenericCell Value Value
-}
type PerhapsFunction = String

data Cell = Cell Adicity PerhapsFunction 
          | Variad (Adicity -> PerhapsFunction)

contextualize :: Adicity -> Cell -> (Adicity, PerhapsFunction)
contextualize _ (Cell adicity x) = (adicity, x)
contextualize adicity (Variad f) = (adicity, f adicity)

-- for intermediate testing purposes only
instance Show (Cell) where 
    show (Cell _ x) = x
    show (Variad f) = f Monadic

nilad :: Value -> Cell
nilad = Cell Niladic . show

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
