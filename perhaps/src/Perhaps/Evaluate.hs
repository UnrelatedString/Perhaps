module Perhaps.Evaluate
    ( tokens,
      verboseTokens,
      toSuffix
    ) where

import Perhaps.Data
    ( Token (Literal, Primitive, Operator, ReplaceWithEverything),
      Value (Number, Char, List),
      Function (Primitive, Literal, Derived, Train)
      Primitive,
      Operator, opArity,
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

toPrefix :: [Token] -> [Token]
toPrefix (Operator x:t) = Operator x:toPrefix (ReplaceWithEverything:t)
toPrefix (x:Operator y:t) = Operator y:toPrefix (x:t)
toPrefix (x:t) = x:toPrefix t
toPrefix [] = []

data PossiblyIncompleteFunction = Primitive Primitive
                                | Literal Value
                                | ReplaceWithEverything
                                | Derived Operator [PossiblyIncompleteFunction]

operate :: [Token] -> [Function]
operate = fill . reverse . foldl step [repeat ReplaceWithEverything] . reverse
    where step :: [Maybe Function] -> Token -> [Maybe Function]
          step stack (Literal tok) = Just (Literal tok):stack
          step stack (Primitive tok) = Just (Primitive tok):stack
          step stack ReplaceWithEverything = ReplaceWithEverything:stack
          step stack (Operator tok) = Just (Derived tok $
                                          take (opArity tok) stack) :
                                          drop (opArity tok) stack
          fill :: [PossiblyIncompleteFunction] -> [Function]
          fill
