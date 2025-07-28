{-
 - SPDX-FileCopyrightText: 2020-2022 UnrelatedString <https://github.com/UnrelatedString> and other Perhaps contributors
 -
 - SPDX-License-Identifier: BSD-3-Clause
 -}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}

module Perhaps.Evaluate
    ( tokens,
      verboseTokens,
      toPostfix,
      swapBy,
      operate,
      fillGaps,
      testF
    ) where

import Perhaps.Data
    ( Token (CellT, OperatorT),
      Value (Number, Char, List),
      nilad,
      contextualize,
      FirstPassCell (FullFunction, PartialFunction),
      hole,
      Cell (Cell, Variad),
      PerhapsFunction,
      nilad,
      Adicity (Niladic, Monadic, Dyadic),
      Operator (Operator),
      operatorIsUnary,
      derive,
      Number,
      Arguments,
      left,
      right,
      original,
      onLeft,
      onRight,
      onOriginal,
      fill
    )

import Perhaps.Operator
    ( lookOp
    )

import Perhaps.Primitive
    ( primitiveLookup
    )

import Data.Char (isDigit, isUpper)
import Data.Maybe (isNothing)
import Data.Foldable (toList) --could just foldr (:) [] but that's less readable
import Control.Monad (join)

tokens :: String -> [[Token]]
tokens = undefined

-- I think I'll elect to use an escape/substitute for newlines within strings in both syntaxes
verboseTokens :: String -> [[Token]]
verboseTokens = map (map parseVerboseToken . tokenizeLine "") . lines
    where tokenizeLine :: String -> String -> [String]
          tokenizeLine "" "" = []
          tokenizeLine tok "" = pure tok
          tokenizeLine "" (' ':rest) = tokenizeLine "" rest
          tokenizeLine tok (' ':rest) = tok : tokenizeLine "" rest
          tokenizeLine "" ('"':rest) = munchString "\"" rest
          tokenizeLine tok ('"':rest) = tok : munchString "\"" rest
          tokenizeLine tok (h:rest) = tokenizeLine (h:tok) rest
          munchString :: String -> String -> [String]
          munchString tok "" = pure tok
          munchString tok ('"':rest) = ('"':tok) : tokenizeLine "" rest
          munchString tok (h:rest) = munchString (h:tok) rest
          parseVerboseToken :: String -> Token
          parseVerboseToken tok
              | all isDigit t = CellT $ nilad $ Number $ fromInteger $ read t
              | h == '"' = CellT $ nilad $ List $ map Char $ tail t
              | isUpper h = OperatorT $ lookOp t
              | otherwise = CellT $ primitiveLookup t
              where t = case tok of '"':r -> reverse r
                                    _ -> reverse tok
                    h = head t

swapBy :: forall a. (a -> Bool) -> [a] -> [Maybe a]
swapBy f = swapBy' Nothing
    where swapBy' :: Maybe a -> [a] -> [Maybe a]
          swapBy' h (x:t)
              | f x = Just x : swapBy' h t
              | otherwise = h : swapBy' (Just x) t
          swapBy' h [] = [h]

isOperator :: Token -> Bool
isOperator (OperatorT _) = True
isOperator _ = False

tokenIsUnaryOperator :: Token -> Bool
tokenIsUnaryOperator (OperatorT op) = operatorIsUnary op
tokenIsUnaryOperator _ = False

toPostfix :: [Token] -> [Maybe Token]
toPostfix = map join . swapBy (any tokenIsUnaryOperator) . reverse . swapBy isOperator . reverse

-- TODO: care about extra missing arguments (consume more lines? supply primitives?)
-- I... think binds would go in here when I do those?
operate :: [Maybe Token] -> [FirstPassCell]
operate = reverse . foldl operate' []
    where operate' :: [FirstPassCell] -> Maybe Token -> [FirstPassCell]
          operate' stack (Just (OperatorT op)) = derive op stack
          operate' stack (Just (CellT x)) = FullFunction x : stack
          operate' stack Nothing = hole : stack

fillGaps :: [FirstPassCell] -> [Cell]
fillGaps (PartialFunction fill : t) = [fill $ tissueify $ fillGaps t] -- leading first for implementation convenience; can't remember the exact logic of the swap system well enough to say if this helps or hurts exotic combinations of unary and higher-ary operators on both edges
fillGaps es = reverse $ foldl fillGaps' [] es
    where fillGaps' :: [Cell] -> FirstPassCell -> [Cell]
          fillGaps' fs (PartialFunction fill) = [fill $ tissueify $ reverse fs]
          fillGaps' fs (FullFunction x) = x : fs

tissueify :: [Cell] -> Cell
tissueify cells = Variad \adicity -> runTissue adicity cells

runTissue :: Adicity -> [Cell] -> PerhapsFunction
runTissue = (((. onOriginal left).).) $ (.) <$> runTissue' <*> fmap . contextualize
    where runTissue' :: Adicity -> [(Adicity, PerhapsFunction)] -> PerhapsFunction
          runTissue' Niladic ((Niladic, nilad) : tail) = runTissue' Monadic tail . fill . nilad
          runTissue' Niladic l = runTissue' Monadic l . const (fill $ Number 0)
          runTissue' Monadic ((Dyadic, dyad) : (Monadic, monad) : tail) = runTissue' Monadic tail . onLeft (dyad . onRight monad)
          runTissue' Dyadic ((Dyadic, dyad1) : (Dyadic, dyad2) : (Niladic, nilad) : tail) = runTissue' Dyadic tail . onLeft (dyad2 . onRight nilad) . onLeft dyad1 -- remember this goes poof too if I do binds
          runTissue' Dyadic ((Dyadic, dyad1) : (Dyadic, dyad2) : tail) = runTissue' Dyadic tail . onLeft (dyad1 . onRight dyad2 . onLeft original)
          runTissue' adicity ((Dyadic, dyad) : (Niladic, nilad) : tail) = runTissue' adicity tail . onLeft (dyad . onRight nilad)
          runTissue' adicity ((Niladic, nilad) : (Dyadic, dyad) : tail) = runTissue' adicity tail . onLeft (dyad . onLeft nilad)
          runTissue' adicity ((Monadic, monad) : tail) = runTissue' adicity tail . onLeft monad
          runTissue' adicity ((Dyadic, dyad) : tail) = runTissue' adicity tail . onLeft dyad
          runTissue' adicity ((Niladic, nilad) : tail) = error "halt and catch fire"
          runTissue' _ [] = left

testF = (snd.).flip contextualize.tissueify.fillGaps.operate.toPostfix.head.verboseTokens
