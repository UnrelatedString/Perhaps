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
    ( Token (LiteralT, PrimitiveT, OperatorT),
      Value (Number, Char, List),
      Expression (PrimitiveE, LiteralE, DerivedE),
      FirstPassFunction (FullFunction, PartialFunction),
      PerhapsFunction (PerhapsFunction),
      Primitive (Primitive),
      Operator (Operator),
      Number,
      integerMaybe
    ) where

import Data.Ratio (Rational, numerator, denominator)
--import Data.Complex (Complex, realPart, imagPart)

data Token = LiteralT Value
           | PrimitiveT Primitive
           | OperatorT Operator deriving (Show)

data Expression = PrimitiveE Primitive
               | LiteralE Value
               | DerivedE (Derived (Maybe Expression))
               | LeftBindE (Maybe Expression) (Maybe Expression)
               | RightBindE (Maybe Expression) (Maybe Expression) deriving (Show)

data FirstPassFunction = FullFunction PerhapsFunction
                       | PartialFunction (PerhapsFunction -> PerhapsFunction)

data GenericPerhapsFunction a b = PerhapsFunction Adicity (a -> b) -- xd

instance Category GenericPerhapsFunction where
    id = PerhapsFunction id
    (PerhapsFunction x) . (PerhapsFunction y) = x . y

type PerhapsFunction = GenericPerhapsFunction Value Value

-- Syntactic adicity, not semantic adicity
data Adicity = Niladic | Monadic | Dyadic | Variadic (Adicity -> Adicity)

class Adic a where
    adicity :: a -> Adicity

-- TODO: flagged lists -- just using lists for convenience at moment
-- replace before even implementing choice
data Value = Number Number | Char Char | List [Value] deriving (Show)

-- TODO: replace with symbolic math lmao, don't really want to approximate radicals/pi
type Number = Rational

integerMaybe :: Number -> Maybe Integer
integerMaybe x
    | denominator x == 1 = Just $ numerator x
    | otherwise = Nothing

-- placeholder
type Primitive = Primitive Function

data Operator = Operator Bool ([Maybe Expression] -> (FirstPassFunction, [Maybe Expression]))
