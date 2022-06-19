{-# LANGUAGE BlockArguments #-}

module Perhaps.Operator
    ( lookOp
    ) where

import Perhaps.Data
    ( Operator (Operator),
      FirstPassCell (FullFunction, PartialFunction),
      Cell (Cell),
      Adicity (Niladic, Monadic, Dyadic))

unary :: (Cell -> Cell) -> Operator
unary f = Operator True \(x:es) -> (d x, es)
    where d :: FirstPassCell -> FirstPassCell
          d (FullFunction x) = FullFunction $ f x
          d (PartialFunction fill) = PartialFunction $ f . fill

binary :: (Cell -> Cell -> Cell) -> Operator
binary f = Operator False \(y:x:es) -> (d x y, es)
    where d :: FirstPassCell -> FirstPassCell -> FirstPassCell -- maybe refactor this back into fixed lists later?
          d (FullFunction x) (FullFunction y) = FullFunction $ f x y
          d (FullFunction x) (PartialFunction fill) = PartialFunction $ f x . fill
          d (PartialFunction fill) (FullFunction y) = PartialFunction $ flip f y . fill
          d (PartialFunction fill1) (PartialFunction fill2) = error "Multiple missing arguments behavior unimplemented"

ternary :: (Cell -> Cell -> Cell -> Cell) -> Operator
ternary f = Operator False \(z:y:x:es) -> (d x y z, es)
    where d :: FirstPassCell -> FirstPassCell -> FirstPassCell -> FirstPassCell
          d (FullFunction x) (FullFunction y) (FullFunction z) = FullFunction $ f x y z
          d (FullFunction x) (FullFunction y) (PartialFunction fill) = PartialFunction $ f x y . fill
          d (FullFunction x) (PartialFunction fill) (FullFunction z) = PartialFunction $ (\y -> f x y z) . fill
          d (PartialFunction fill) (FullFunction y) (FullFunction z) = PartialFunction $ (\x -> f x y z) . fill
          d _ _ _ = error "Multiple missing arguments behavior unimplemented"

-- unified lookup across both syntaxes because lmao why not
lookOp :: String -> Operator -- ba dum tss 🥁
lookOp "A" = unary placeholderA
lookOp "B" = binary placeholderB
lookOp "D" = binary placeholderD


placeholderA :: Cell -> Cell
placeholderA x = Cell Monadic $ "(" ++ show x ++ " A)"

placeholderB :: Cell -> Cell -> Cell
placeholderB x y = Cell Monadic $ "(" ++ show x ++ " B " ++ show y ++ ")"

placeholderD :: Cell -> Cell -> Cell
placeholderD x y = Cell Dyadic $ "(" ++ show x ++ " D " ++ show y ++ ")"
