{-
 - SPDX-FileCopyrightText: 2020-2021 UnrelatedString <https://github.com/UnrelatedString> and other Perhaps contributors
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
      Number
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
          operate' stack (Just (OperatorT op)) = d : rest
              where (d, rest) = derive op stack
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
runTissue = (.) <$> runTissue' <*> fmap . contextualize
    where runTissue' :: Adicity -> [(Adicity, PerhapsFunction)] -> PerhapsFunction
          runTissue' Niladic ((Niladic, nilad) : tail) = nilad ++ " " ++ runTissue' Monadic tail -- how do i represent this notationally lmao
          runTissue' Niladic l = "0 " ++ runTissue' Monadic l
          runTissue' Monadic ((Dyadic, dyad) : (Monadic, monad) : tail) = dyad ++ "<" ++ monad ++ " " ++ runTissue' Monadic tail
          -- cba to mock up dyadic 2,2,0 lmao
          runTissue' Dyadic ((Dyadic, dyad1) : (Dyadic, dyad2) : tail) = dyad1 ++ "<" ++ dyad2 ++ " " ++ runTissue' Dyadic tail
          runTissue' adicity ((Dyadic, dyad) : (Niladic, nilad) : tail) = dyad ++ nilad ++ " " ++ runTissue' adicity tail
          runTissue' adicity ((Niladic, nilad) : (Dyadic, dyad) : tail) = nilad ++ dyad ++ " " ++ runTissue' adicity tail
          runTissue' adicity ((Monadic, monad) : tail) = monad ++ " " ++ runTissue' adicity tail
          runTissue' adicity ((Dyadic, dyad) : tail) = dyad ++ " " ++ runTissue' adicity tail
          runTissue' adicity ((Niladic, nilad) : tail) = "halt and catch fire"
          runTissue' _ [] = []

testF = flip contextualize.tissueify.fillGaps.operate.toPostfix.head.verboseTokens
