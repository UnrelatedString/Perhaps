{-
 - SPDX-FileCopyrightText: 2020-2022 UnrelatedString <https://github.com/UnrelatedString> and other Perhaps contributors
 -
 - SPDX-License-Identifier: BSD-3-Clause
 -}

module Perhaps.Invoke
    ( invoke
    ) where

import System.Environment (getArgs)
import Perhaps.Data
    ( Arguments (Arguments),
      fill,
      fromLeftRight,
      Value (Number), -- think of better default value?
      forceReadValue,
      Adicity (Niladic, Monadic, Dyadic),
      stringMaybe
    )
import Perhaps.Evaluate (testF) -- maybe I should rename that or something

evaluationParameters :: [Value] -> (Adicity, Arguments)
evaluationParameters [] = (Niladic, fill (Number 0))
evaluationParameters [x] = (Monadic, fill x)
evaluationParameters (x:y:_) = (Dyadic, fromLeftRight x y)

splitFlags :: [String] -> ([String], [String])
splitFlags = fmap trimExplicitSeparator . span isFlag
    where isFlag s = head s == '-' && s /= "--"
          trimExplicitSeparator ("--":t) = t
          trimExplicitSeparator s = s

substituteStdin :: String -> IO String
substituteStdin "-" = getContents
substituteStdin s = return s

invoke :: IO ()
invoke = do (flags, arguments) <- splitFlags <$> getArgs
            if notElem "--verbose" flags && notElem "-v" flags then fail "SBCS syntax not implemented" else return ()
            (program : fullArguments) <- mapM substituteStdin arguments
            let result = uncurry (testF program) $ evaluationParameters $ forceReadValue <$> fullArguments
            case stringMaybe result of
                Just string -> putStrLn string
                _ -> print result