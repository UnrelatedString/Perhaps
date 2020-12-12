module Perhaps.Data
    ( Token (Literal, Primitive, Operator, ReplaceWithEverything),
      Value (Number, Char, List),
      Primitive,
      Operator,
      Number,
      integerMaybe
    ) where

import Data.Ratio (Rational, numerator, denominator)
--import Data.Complex (Complex, realPart, imagPart)

data Token = Literal Value
           | Primitive Primitive
           | Operator Operator
           | ReplaceWithEverything deriving (Show)


-- TODO: flagged lists -- just using lists for convenience at moment
-- replace before even implementing choice
data Value = Number Number | Char Char | List [Value] deriving (Show)

-- TODO: replace with symbolic math lmao, don't really want to approximate radicals/pi
type Number = Rational

integerMaybe :: Number -> Maybe Integer
integerMaybe x
    | denominator x == 1 = Just $ numerator x
    | otherwise = Nothing

-- all of these are also placeholders

type Primitive = String
type Operator = String