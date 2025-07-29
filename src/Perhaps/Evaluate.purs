-- SPDX-FileCopyrightText: 2020-2025 UnrelatedString <https://github.com/UnrelatedString> and other Perhaps contributors
--
-- SPDX-License-Identifier: BSD-3-Clause

module Perhaps.Evaluate
  ( tokens
  , verboseTokens
  , toPostfix
  , pullIf
  , operate
  , fillGaps
  , testF
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.List
  ( List(Nil)
  , (:)
  , reverse
  , singleton
  )

import Perhaps.Core
  ( Token(..)
  , FirstPassCell(..)
  )

pullIf :: forall a. (a -> Boolean) -> List a -> List (Maybe a)
pullIf f = pullIf' Nothing
  where
  pullIf' :: Maybe a -> List a -> List (Maybe a)
  pullIf' h (x:t)
    | f x = Just x : pullIf' h t
    | otherwise = h : pullIf' (Just x) t
  pullIf' h Nil = singleton h

isOperator :: Token -> Boolean
isOperator (OperatorTok _) = True
isOperator _ = False

tokenIsUnaryOperator :: Token -> Boolean
tokenIsUnaryOperator (OperatorT { unary }) = unary
tokenIsUnaryOperator _ = False

toPostfix :: List Token -> List (Maybe Token)
toPostfix = map join <<< pullIf (any tokenIsUnaryOperator) <<< reverse <<< pullIf isOperator <<< reverse

-- TODO: care about extra missing arguments (consume more lines? supply primitives?)
-- I... think binds would go in here when I do those?
operate :: List (Maybe Token) -> List FirstPassCell
operate = reverse <<< foldl operate' Nil
  where
  operate' :: List FirstPassCell -> Maybe Token -> List FirstPassCell
  operate' stack (Just (OperatorT { operate: o })) = o op stack
  operate' stack (Just (CellT x)) = FullFunction x : stack
  operate' stack Nothing = hole : stack

-- fillGaps :: [FirstPassCell] -> [Cell]
-- fillGaps (PartialFunction fill : t) = [fill $ tissueify $ fillGaps t] -- leading first for implementation convenience; can't remember the exact logic of the swap system well enough to say if this helps or hurts exotic combinations of unary and higher-ary operators on both edges
-- fillGaps es = reverse $ foldl fillGaps' [] es
--   where
--   fillGaps' :: [Cell] -> FirstPassCell -> [Cell]
--   fillGaps' fs (PartialFunction fill) = [fill $ tissueify $ reverse fs]
--   fillGaps' fs (FullFunction x) = x : fs

-- tissueify :: [Cell] -> Cell
-- tissueify cells = Variad \adicity -> runTissue adicity cells

-- runTissue :: Adicity -> [Cell] -> PerhapsFunction
-- runTissue = (((. onOriginal left).).) $ (.) <$> runTissue' <*> fmap . contextualize
--   where
--   runTissue' :: Adicity -> List (Tuple Adicity PerhapsFunction) -> PerhapsFunction
--   runTissue' Niladic ((Niladic, nilad) : tail) = runTissue' Monadic tail . fill . nilad
--   runTissue' Niladic l = runTissue' Monadic l . const (fill $ Number 0)
--   runTissue' Monadic ((Dyadic, dyad) : (Monadic, monad) : tail) = runTissue' Monadic tail . onLeft (dyad . onRight monad)
--   runTissue' Dyadic ((Dyadic, dyad1) : (Dyadic, dyad2) : (Niladic, nilad) : tail) = runTissue' Dyadic tail . onLeft (dyad2 . onRight nilad) . onLeft dyad1 -- remember this goes poof too if I do binds
--   runTissue' Dyadic ((Dyadic, dyad1) : (Dyadic, dyad2) : tail) = runTissue' Dyadic tail . onLeft (dyad1 . onRight dyad2 . onLeft original)
--   runTissue' adicity ((Dyadic, dyad) : (Niladic, nilad) : tail) = runTissue' adicity tail . onLeft (dyad . onRight nilad)
--   runTissue' adicity ((Niladic, nilad) : (Dyadic, dyad) : tail) = runTissue' adicity tail . onLeft (dyad . onLeft nilad)
--   runTissue' adicity ((Monadic, monad) : tail) = runTissue' adicity tail . onLeft monad
--   runTissue' adicity ((Dyadic, dyad) : tail) = runTissue' adicity tail . onLeft dyad
--   runTissue' adicity ((Niladic, nilad) : tail) = error "halt and catch fire"
--   runTissue' _ Nil = left

-- testF = (snd.).flip contextualize.tissueify.fillGaps.operate.toPostfix.head.verboseTokens
