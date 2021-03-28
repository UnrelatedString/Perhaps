{-
 - SPDX-FileCopyrightText: 2020-2021 UnrelatedString <https://github.com/UnrelatedString> and other Perhaps contributors
 -
 - SPDX-License-Identifier: BSD-3-Clause
 -}

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

module Perhaps.Data
    ( Token (LiteralT, PrimitiveT, OperatorT),
      Value (Number, Char, List),
      Expression (PrimitiveE, LiteralE, DerivedE),
      Function (PrimitiveF, LiteralF, DerivedF, TrainF),
      Primitive,
      Operator (Operator),
      lookOp,
      Derived,
      derive,
      Number,
      integerMaybe
    ) where

import Data.Ratio (Rational, numerator, denominator)
--import Data.Complex (Complex, realPart, imagPart)
import Data.FixedList (Nil (Nil), Cons ((:.)))

data Token = LiteralT Value
           | PrimitiveT Primitive
           | OperatorT Operator --deriving (Show)

data Expression = PrimitiveE Primitive
               | LiteralE Value
               | DerivedE (Derived (Maybe Expression)) --deriving (Show)

data Function = PrimitiveF Primitive
              | LiteralF Value
              | DerivedF (Derived Function)
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

-- placeholder
type Primitive = String

data Derived a = forall f. Traversable f => Derived (f a) (f Function -> String)

instance Functor Derived where
    fmap f (Derived args op) = Derived (fmap f args) op
instance Foldable Derived where
    foldr f x (Derived args _) = foldr f x args
instance Traversable Derived where
    sequenceA (Derived args op) = flip Derived op <$> sequenceA args

instance Show (Derived Function) where
    show = derive -- I genuinely don't know where I'm going with this placeholder functionality lmao

derive :: Derived Function -> String
derive (Derived args op) = op args

data Operator = Operator Bool ([Maybe Expression] -> (Derived (Maybe Expression), [Maybe Expression]))

-- TODO: make actual operators and put them in a different file
lookOp :: String -> Operator -- ba dum tss 🥁
lookOp "A" = Operator True (\(e:es) -> (Derived (e:.Nil) (\(f:.Nil) -> "(" ++ show f ++ " A)"), es))
lookOp "B" = Operator False (\(x:y:es) -> (Derived (x:.y:.Nil) (\(x:.y:.Nil) -> "(" ++ show x ++ " B " ++ show y++")"), es))
lookOp "D" = Operator False (\(x:y:es) -> (Derived (x:.y:.Nil) (\(x:.y:.Nil) -> "(" ++ show x ++ " D " ++ show y++")"), es))
