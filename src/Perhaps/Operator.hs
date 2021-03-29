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

unary :: (Function -> String) -> Operator
unary f = Operator True (\(x:es) -> (Derived d (x:.Nil), es))
    where d :: Cons Nil Function -> String
          d (x :. Nil) = f x

binary :: (Function -> Function -> String) -> Operator
binary f = Operator True (\(y:x:es) -> (Derived d (x:.y:.Nil), es))
    where d :: Cons (Cons Nil) Function -> String
          d (x :. y :. Nil) = f x y

lookOp :: String -> Operator -- ba dum tss 🥁
lookOp "A" = unary placeholderA
lookOp "B" = binary placeholderB
lookOp "D" = binary placeholderD


-- TODO: actual operators, replace String with something
placeholderA :: Function -> String
placeholderA x = "(" ++ show x ++ " A)"

placeholderB :: Function -> Function -> String
placeholderB x y = "(" ++ show x ++ " B " ++ show y ++ ")"

placeholderD :: Function -> Function -> String
placeholderD x y = "(" ++ show x ++ " D " ++ show y ++ ")"
