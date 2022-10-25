module Perhaps.Misc
    ( antidiagonals,
      antidiagonalize
    ) where

import Data.Maybe (catMaybes)

antidiagonals :: [[a]] -> [[a]]
antidiagonals = antidiagonals' []
    where antidiagonals' prefix (h:t) =
              let (antidiagonal, tails) = unconses prefix
              in  antidiagonal : antidiagonals' (h:tails) t
          unconses = unzip . catMaybes . map uncons
          uncons (h:t) = Just (h,t)
          uncons [] = Nothing

antidiagonalize = concat . antidiagonals
