module Perhaps.Operator
    ( lookOp
    ) where

import Perhaps.Data
    ( Operator (Operator),
      Derived (Derived),
      Function )

import Data.FixedList
    ( Nil (Nil),
      Cons ((:.)) )

unary :: String -> (Function -> String) -> Operator
unary name f = Operator name True (\(x:es) -> (Derived name d (x:.Nil), es))
    where d :: Cons Nil Function -> String -- I could use Identity, but the golfer in me says :.Nil is three bytes shorter
          d (x :. Nil) = f x

binary :: String -> (Function -> Function -> String) -> Operator
binary name f = Operator name True (\(y:x:es) -> (Derived name d (x:.y:.Nil), es))
    where d :: Cons (Cons Nil) Function -> String
          d (x :. y :. Nil) = f x y

--Is this a job for Template Haskell... or the C preprocessor?
lookOp :: String -> Operator -- ba dum tss 🥁
lookOp "A" = unary "A" placeholderA
lookOp "B" = binary "B" placeholderB
lookOp "D" = binary "B" placeholderD


-- TODO: actual operators, replace String with something
placeholderA :: Function -> String
placeholderA x = "(" ++ show x ++ " A)"

placeholderB :: Function -> Function -> String
placeholderB x y = "(" ++ show x ++ " B " ++ show y ++ ")"

placeholderD :: Function -> Function -> String
placeholderD x y = "(" ++ show x ++ " D " ++ show y ++ ")"
