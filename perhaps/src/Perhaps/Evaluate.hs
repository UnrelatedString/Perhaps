module Perhaps.Evaluate
    ( tokens,
      verboseTokens,
      toPostfix,
      swapBy,
      operate
    ) where

import Perhaps.Data
    ( Token (LiteralT, PrimitiveT, OperatorT),
      Value (Number, Char, List),
      Function (PrimitiveF, LiteralF, DerivedF, TrainF),
      Primitive,
      Operator, opArity,
      Number)

import Data.Char (isDigit, isUpper)
import Data.Maybe (isNothing)
import Data.Foldable (toList)
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

swapBy :: (a -> Bool) -> [a] -> [Maybe a]
swapBy f = swapBy' Nothing
    where swapBy' h (x:t)
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

data Expression = PrimitiveE Primitive
                | LiteralE Value
                | DerivedE Operator [Maybe Expression] deriving (Show)

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
