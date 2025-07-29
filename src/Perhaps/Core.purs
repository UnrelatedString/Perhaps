-- SPDX-FileCopyrightText: 2020-2025 UnrelatedString <https://github.com/UnrelatedString> and other Perhaps contributors
--
-- SPDX-License-Identifier: BSD-3-Clause

module Perhaps.Core
  ( Token (CellTok, OperatorTok)
  , Value (VNumber, VChar, VList)
  , FirstPassCell (FullFunction, PartialFunction)
  , hole
  , Cell (Cell, Variad)
  , nilad
  , monad
  , dyad
  , contextualize
  , Adicity (Niladic, Monadic, Dyadic)
  , Operator (Operator)
  , PerhapsFunction
  , Arguments (Arguments)
  ) where

import Prelude

import Data.List (List)
import Data.Tuple (Tuple(..))

-- Syntactic adicity, not semantic adicity
data Adicity = Niladic | Monadic | Dyadic

type PerhapsFunction = Value -> Value

data Cell
  = Cell Adicity PerhapsFunction
  | Variad (Adicity -> PerhapsFunction)

contextualize :: Adicity -> Cell -> Tuple Adicity PerhapsFunction
contextualize _ (Cell adicity x) = Tuple adicity x
contextualize adicity (Variad f) = Tuple adicity (f adicity)

nilad :: Value -> Cell
nilad = Cell Niladic <<< const

monad :: (Value -> Value) -> Cell
monad f = Cell Monadic \Arguments { left } -> f left

dyad :: (Value -> Value -> Value) -> Cell
dyad f = Cell Dyadic \Arguments { left, right } -> f left right

data Token
  = CellTok Cell
  | OperatorTok Operator

data FirstPassCell
  = FullFunction Cell
  | PartialFunction (Cell -> Cell)

hole :: FirstPassCell
hole = PartialFunction identity

data Value = VNumber Number | VChar Char | VList (List Value)

data Operator = Operator
  { unary :: Boolean
  , operate :: List FirstPassCell -> List FirstPassCell
  }

-- cyclic imports are illegal :(
-- um no shit?? girlllll how the fuck were you me
-- wait no i've bitched about this in purescript too lol

data Arguments = Arguments
  { left :: Value
  , right :: Value
  , original :: Value
  }
