module Test.Main where

import Prelude
import Effect (Effect)
import Test.Spec.Discovery (discover)
import Test.Spec.Runner (run)
import Test.Spec.Reporter.Console (consoleReporter)

main :: Effect Unit
main = discover "Test\\.Spec\\..*Spec" >>= run [consoleReporter]