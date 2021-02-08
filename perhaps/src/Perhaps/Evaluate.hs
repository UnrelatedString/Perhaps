module Perhaps.Evaluate
    ( tokens,
      verboseTokens,
      opSwap
    ) where

import Perhaps.Data
    ( Token (Literal, Primitive, Operator),
      Value (Number, Char, List),
     -- Function (Primitive, Literal, Derived, Train),
      Primitive,
      Operator, opArity,
      Number)

import Data.Char (isDigit, isUpper)
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
              | all isDigit t = Literal $ Number $ fromInteger $ read t
              | h == '"' = Literal $ List $ map Char $ tail t
              | isUpper h = Operator t
              | otherwise = Primitive t
              where t = case tok of '"':r -> reverse r
                                    _ -> reverse tok
                    h = head t

swaps :: (a -> Bool) -> [a] -> [Maybe a]
swaps f = swaps' Nothing
    where swaps' h (x:t)
              | f x = Just x : swaps' h t
              | otherwise = h : swaps' (Just x) t
          swaps' h [] = [h]

isOperator :: Token -> Bool
isOperator (Operator _) = True
isOperator _ = False

isUnary :: Token -> Bool
isUnary (Operator x) = opArity x == 1
isUnary _ = False

opSwap :: [Token] -> [Maybe Token]
opSwap = map join . swaps (any isUnary) . reverse . swaps isOperator . reverse
