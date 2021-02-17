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
    ( Token (LiteralT, PrimitiveT, OperatorT),
      Value (Number, Char, List),
      Expression (PrimitiveE, LiteralE, DerivedE),
      Function (PrimitiveF, LiteralF, DerivedF, TrainF),
      Primitive,
      Operator, opArity,
      Number)

import Data.Char (isDigit, isUpper)
import Data.Maybe (isNothing)
import Data.Foldable (toList) --could just foldr (:) [] but that's less readable
import Control.Monad (join)

tokens :: String -> [[Token]]
tokens = undefined

-- I think I'll elect to use an escape for newlines within strings
verboseTokens :: String -> [[Token]]
verboseTokens = map (map parseVerboseToken . tokenizeLine "") . lines
    where tokenizeLine :: String -> String -> [String]
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
              | all isDigit t = LiteralT $ Number $ fromInteger $ read t
              | h == '"' = LiteralT $ List $ map Char $ tail t
              | isUpper h = OperatorT t
              | otherwise = PrimitiveT t
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

isUnary :: Token -> Bool
isUnary (OperatorT x) = opArity x == 1
isUnary _ = False

toPostfix :: [Token] -> [Maybe Token]
toPostfix = map join . swapBy (any isUnary) . reverse . swapBy isOperator . reverse

-- TODO: care about extra missing arguments (consume more lines? supply primitives?)
operate :: [Maybe Token] -> [Expression]
operate = reverse . (>>= toList) . foldl operate' [] -- no top level Nothing
    where operate' :: [Maybe Expression] -> Maybe Token -> [Maybe Expression]
          operate' stack (Just (OperatorT x)) = Just (DerivedE x $ reverse args) : rest
              where a = opArity x
                    args = take a stack
                    rest = drop a stack
          operate' stack (Just (PrimitiveT x)) = Just (PrimitiveE x) : stack
          operate' stack (Just (LiteralT x)) = Just (LiteralE x) : stack
          operate' stack Nothing = Nothing : stack

completeMaybe :: Maybe Expression -> Maybe Function
completeMaybe Nothing = Nothing
completeMaybe (Just (DerivedE x args)) = fmap (DerivedF x) $ sequence $ map completeMaybe args
completeMaybe (Just (PrimitiveE x)) = Just $ PrimitiveF x
completeMaybe (Just (LiteralE x)) = Just $ LiteralF x

fillGap :: [Function] -> Maybe Expression -> Function
fillGap t Nothing = TrainF t
fillGap t (Just (DerivedE x args)) = DerivedF x $ map (fillGap t) args
fillGap t (Just (PrimitiveE x)) = PrimitiveF x
fillGap t (Just (LiteralE x)) = LiteralF x

fillGaps :: [Expression] -> [Function]
fillGaps (DerivedE x args : t)
    | isNothing $ completeMaybe $ Just $ DerivedE x args = [DerivedF x $ map (fillGap $ fillGaps t) args]
fillGaps es = reverse $ foldl fillGaps' [] es
    where fillGaps' :: [Function] -> Expression -> [Function]
          fillGaps' fs (DerivedE x args) =
              case completeMaybe (Just $ DerivedE x args) of
                  Nothing -> [DerivedF x $ map (fillGap $ reverse fs) args]
                  Just f -> f : fs
          fillGaps' fs (PrimitiveE x) = PrimitiveF x : fs
          fillGaps' fs (LiteralE x) = LiteralF x : fs

testF = fillGaps.operate.toPostfix.head.verboseTokens
