{-
 - SPDX-FileCopyrightText: 2020-2021 UnrelatedString <https://github.com/UnrelatedString> and other Perhaps contributors
 -
 - SPDX-License-Identifier: BSD-3-Clause
 -}

module Perhaps.Data
    ( Token (LiteralT, PrimitiveT, OperatorT),
      Value (Number, Char, List),
      Function (PrimitiveF, LiteralF, DerivedF, TrainF),
      Primitive,
      Operator,
      Number,
      integerMaybe,
      opArity
    ) where

import Data.Ratio (Rational, numerator, denominator)
--import Data.Complex (Complex, realPart, imagPart)

data Token = LiteralT Value
           | PrimitiveT Primitive
           | OperatorT Operator deriving (Show)

data Function = PrimitiveF Primitive
              | LiteralF Value
              | DerivedF Operator [Function]
              | TrainF [Function] deriving (Show)

-- TODO: flagged lists -- just using lists for convenience at moment
-- replace before even implementing choice
data Value = Number Number | Char Char | List [Value] deriving (Show)

-- TODO: replace with symbolic math lmao, don't really want to approximate radicals/pi
type Number = Rational

integerMaybe :: Number -> Maybe Integer
integerMaybe x
    | denominator x == 1 = Just $ numerator x
    | otherwise = Nothing

-- all of these are also placeholders

type Primitive = String
type Operator = String


opArity :: Operator -> Int
opArity = length -- PHP moment
