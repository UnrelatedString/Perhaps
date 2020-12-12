module Perhaps.Tokenize
    ( tokens,
      verboseTokens
    ) where

import Perhaps.Data
    ( Token (Literal, Primitive, Operator),
      Value (Number, Char, List),
      Primitive,
      Operator,
      Number)

tokens :: String -> [[Token]]
tokens = undefined

-- I think I'll elect to use an escape for newlines within strings
verboseTokens :: String -> [[Token]]
verboseTokens = map (tokenizeLine "") . lines
    where tokenizeLine :: String -> String -> [Token]
          tokenizeLine tok "" = pure$Primitive$reverse tok
          tokenizeLine "" (' ':rest) = tokenizeLine "" rest
          tokenizeLine tok (' ':rest) = (Primitive$reverse tok) : tokenizeLine "" rest
          tokenizeLine "" ('"':rest) = munchString "\"" rest
          tokenizeLine tok ('"':rest) = (Primitive$reverse tok) : munchString "\"" rest
          tokenizeLine tok (h:rest) = tokenizeLine (h:tok) rest
          munchString :: String -> String -> [Token]
          munchString tok "" = pure$Literal$List$map Char$reverse tok
          munchString tok ('"':rest) = (Literal$List$map Char$reverse$'"':tok) : tokenizeLine "" rest
          munchString tok (h:rest) = munchString (h:tok) rest