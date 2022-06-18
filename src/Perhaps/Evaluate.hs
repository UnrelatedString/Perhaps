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
      FirstPassFunction (FullFunction, PartialFunction),
      Function (Function),
      Primitive,
      Operator (Operator),
      Number
    )

import Perhaps.Operator
    ( lookOp )

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
              | all isDigit t = LiteralT $ Number $ fromInteger $ read t
              | h == '"' = LiteralT $ List $ map Char $ tail t
              | isUpper h = OperatorT $ lookOp t
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
isUnary (OperatorT (Operator _ u _)) = u
isUnary _ = False

toPostfix :: [Token] -> [Maybe Token]
toPostfix = map join . swapBy (any isUnary) . reverse . swapBy isOperator . reverse

-- TODO: care about extra missing arguments (consume more lines? supply primitives?)
operate :: [Maybe Token] -> [Expression]
operate = reverse . (>>= toList) . foldl operate' [] -- no top level Nothing
    where operate' :: [Maybe Expression] -> Maybe Token -> [Maybe Expression]
          operate' stack (Just (OperatorT (Operator _ _ op))) = Just (DerivedE d) : rest
              where (d, rest) = op stack
          operate' stack (Just (PrimitiveT x)) = Just (PrimitiveE x) : stack
          operate' stack (Just (LiteralT x)) = Just (LiteralE x) : stack
          operate' stack Nothing = Nothing : stack

completeMaybe :: Maybe Expression -> Maybe Function
completeMaybe Nothing = Nothing
completeMaybe (Just (DerivedE d)) = DerivedF <$> traverse completeMaybe d
completeMaybe (Just (PrimitiveE x)) = Just $ PrimitiveF x
completeMaybe (Just (LiteralE x)) = Just $ LiteralF x

fillGap :: [Function] -> Maybe Expression -> Function
fillGap t Nothing = TrainF t
fillGap t (Just (DerivedE d)) = DerivedF $ fillGap t <$> d
fillGap t (Just (PrimitiveE x)) = PrimitiveF x
fillGap t (Just (LiteralE x)) = LiteralF x

fillGaps :: [Expression] -> [Function]
fillGaps (DerivedE d : t)
    | isNothing $ completeMaybe $ Just $ DerivedE d = [DerivedF $ fmap (fillGap $ fillGaps t) d]
fillGaps es = reverse $ foldl fillGaps' [] es
    where fillGaps' :: [Function] -> Expression -> [Function]
          fillGaps' fs (DerivedE d) =
              case completeMaybe (Just $ DerivedE d) of
                  Nothing -> [DerivedF $ fmap (fillGap $ reverse fs) d]
                  Just f -> f : fs
          fillGaps' fs (PrimitiveE x) = PrimitiveF x : fs
          fillGaps' fs (LiteralE x) = LiteralF x : fs

testF = fillGaps.operate.toPostfix.head.verboseTokens
