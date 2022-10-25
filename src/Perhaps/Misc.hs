module Perhaps.Misc
    ( antidiagonals,
      antidiagonalize
    ) where

import Data.Maybe (catMaybes)
import Data.List (transpose)

antidiagonals :: [[a]] -> [[a]]
antidiagonals = tail . antidiagonals' []
    where antidiagonals' prefix (h:t) =
              let (antidiagonal, tails) = unconses prefix
              in  antidiagonal : antidiagonals' (h:tails) t
          antidiagonals' list [] = transpose list
          unconses = unzip . catMaybes . map uncons
          uncons (h:t) = Just (h,t)
          uncons [] = Nothing
          

antidiagonalize = concat . antidiagonals
