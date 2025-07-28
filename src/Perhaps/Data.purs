-- SPDX-FileCopyrightText: 2020-2025 UnrelatedString <https://github.com/UnrelatedString> and other Perhaps contributors
--
-- SPDX-License-Identifier: BSD-3-Clause

module Perhaps.Data
  ( Token (CellT, OperatorT)
  , Value (Number, Char, List)
  , stringMaybe
  , forceReadValue
  , FirstPassCell (FullFunction, PartialFunction)
  , hole
  , Cell (Cell, Variad)
  , nilad
  , monad
  , dyad
  , contextualize
  , Adicity (Niladic, Monadic, Dyadic)
  , Operator (Operator)
  , operatorIsUnary
  , derive
  , Number
  , integerMaybe
  , PerhapsFunction
  , Arguments (Arguments)
  ) where



-- Syntactic adicity, not semantic adicity
data Adicity = Niladic | Monadic | Dyadic

data Cell
  = Cell Adicity PerhapsFunction
  | Variad (Adicity -> PerhapsFunction)

contextualize :: Adicity -> Cell -> (Adicity, PerhapsFunction)
contextualize _ (Cell adicity x) = (adicity, x)
contextualize adicity (Variad f) = (adicity, f adicity)

nilad :: Value -> Cell
nilad = Cell Niladic . const

monad :: (Value -> Value) -> Cell
monad = Cell Monadic . (.left)

dyad :: (Value -> Value -> Value) -> Cell
dyad f = Cell Dyadic \Arguments { left = l, right = r } -> f l r

data Token
  = CellTok Cell
  | OperatorTok Operator

data FirstPassCell
  = FullFunction Cell
  | PartialFunction (Cell -> Cell)

hole :: FirstPassCell
hole = PartialFunction id

-- TODO: flagged lists -- just using lists for convenience at moment
-- replace before even implementing choice
-- first class functions Eventually:tm:
data Value = Number Number | Char Char | List ()

data Operator = Operator
  { operatorIsUnary :: Bool
  , derive :: [FirstPassCell] -> [FirstPassCell]
  }

-- cyclic imports are illegal :(
-- um no shit?? girlllll how the fuck were you me


data Arguments = Arguments {
  left :: Value,
  right :: Value,
  original :: Value
}
