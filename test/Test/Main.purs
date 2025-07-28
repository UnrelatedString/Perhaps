module Test.Main where

import Prelude

import Effect (Effect)
import Test.Spec (pending)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.Spec.Reporter.June.Pretty (prettyReporter)

main :: Effect Unit
main = runSpecAndExitProcess [prettyReporter] do
  pending "anything to test :p"
