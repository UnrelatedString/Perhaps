{-
 - SPDX-FileCopyrightText: 2020-2021 UnrelatedString <https://github.com/UnrelatedString> and other Perhaps contributors
 -
 - SPDX-License-Identifier: BSD-3-Clause
 -}

{-# LANGUAGE ScopedTypeVariables #-}

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
    ( Token (AtomT, OperatorT),
      Value (Number, Char, List),
      nilad,
      FirstPassFunction (FullFunction, PartialFunction),
      hole,
      PerhapsFunction,
      nilad,
      Adicity,
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
              | all isDigit t = AtomT $ nilad $ Number $ fromInteger $ read t
              | h == '"' = AtomT $ nilad $ List $ map Char $ tail t
              | isUpper h = OperatorT $ lookOp t
              | otherwise = AtomT $ primitiveLookup t
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
operate :: [Maybe Token] -> [FirstPassFunction]
operate = reverse . foldl operate' []
    where operate' :: [FirstPassFunction] -> Maybe Token -> [FirstPassFunction]
          operate' stack (Just (OperatorT op)) = d : rest
              where (d, rest) = derive op stack
          operate' stack (Just (AtomT x)) = FullFunction x : stack
          operate' stack Nothing = hole : stack

fillGaps :: [FirstPassFunction] -> [PerhapsFunction]
fillGaps (PartialFunction fill : t) = [fill $ trainify $ fillGaps t] -- leading first for implementation convenience; can't remember the exact logic of the swap system well enough to say if this helps or hurts exotic combinations of unary and higher-ary operators on both edges
fillGaps es = reverse $ foldl fillGaps' [] es
    where fillGaps' :: [PerhapsFunction] -> FirstPassFunction -> [PerhapsFunction]
          fillGaps' fs (PartialFunction fill) = [fill $ trainify $ reverse fs]
          fillGaps' fs (FullFunction x) = x : fs

-- adicity argument Later
trainify :: [PerhapsFunction] -> PerhapsFunction
trainify funcs = "[" ++ unwords funcs ++ "]"

testF = fillGaps.operate.toPostfix.head.verboseTokens
