module Perhaps.Operator
    ( lookOp
    ) where

import Perhaps.Data
    ( Operator (Operator),
      FirstPassFunction (FullFunction, PartialFunction),
      Function (Function))

unary :: (Function -> Function) -> Operator
unary name f = Operator name True (\(x:es) -> (Derived name d (x:.Nil), es))
    where d :: Cons Nil Function -> String -- I could use Identity, but the golfer in me says :.Nil is three bytes shorter
          d (x :. Nil) = f x

binary :: (Function -> Function -> Function) -> Operator
binary name f = Operator name False (\(y:x:es) -> (Derived name d (x:.y:.Nil), es))
    where d :: Cons (Cons Nil) Function -> String
          d (x :. y :. Nil) = f x y

lookOp :: String -> Operator -- ba dum tss 🥁
lookOp "A" = unary placeholderA
lookOp "B" = binary placeholderB
lookOp "D" = binary placeholderD


placeholderA :: Function -> Function
placeholderA x = "(" ++ show x ++ " A)"

placeholderB :: Function -> Function -> Function
placeholderB x y = "(" ++ show x ++ " B " ++ show y ++ ")"

placeholderD :: Function -> Function -> Function
placeholderD x y = "(" ++ show x ++ " D " ++ show y ++ ")"
