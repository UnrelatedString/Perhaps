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
      Function (PrimitiveF, LiteralF, DerivedF, TrainF),
      Primitive,
      Operator (Operator),
      Derived (Derived),
      derive,
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

data Function = PrimitiveF Primitive
              | LiteralF Value
              | DerivedF (Derived Function)
              | TrainF [Function]
              | LeftBindF Function Function
              | RightBindF Function Function deriving (Show)

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
type Primitive = String

data Derived a = forall f. (Traversable f, forall b. Show b => Show (f b)) =>
                     Derived String (f Function -> String) (f a)

instance Functor Derived where
    fmap f (Derived name op args) = Derived name op $ fmap f args
instance Foldable Derived where
    foldr f x (Derived _ _ args) = foldr f x args
instance Traversable Derived where
    sequenceA (Derived name op args) = Derived name op <$> sequenceA args

instance Show a => Show (Derived a) where
    show (Derived name _ args) = name ++ " " ++ show args

derive :: Derived Function -> String -- bad name
derive (Derived _ op args) = op args

data Operator = Operator String Bool ([Maybe Expression] -> (Derived (Maybe Expression), [Maybe Expression]))

instance Show Operator where
    show (Operator name _ _) = name
