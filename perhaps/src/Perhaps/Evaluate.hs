module Perhaps.Evaluate
    ( tokens,
      verboseTokens,
      toSuffix
    ) where

import Perhaps.Data
    ( Token (Literal, Primitive, Operator, ReplaceWithEverything),
      Value (Number, Char, List),
      Primitive,
      Operator,
      Number)

import Data.Char (isDigit, isUpper)

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

toSuffix :: [Token] -> [Token]
toSuffix = reverse . flips . reverse
    where flips :: [Token] -> [Token]
          flips (Operator x:t) = Operator x:flips (ReplaceWithEverything:t)
          flips (x:Operator y:t) = Operator y:flips (x:t)
          flips (x:t) = x:flips t
          flips [] = []
