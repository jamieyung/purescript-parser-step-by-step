module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.ParserStepByStep.Step0 as Step0
import Test.ParserStepByStep.Step1 as Step1
import Test.ParserStepByStep.Step2 as Step2
import Test.ParserStepByStep.Step3 as Step3
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  Step0.spec
  Step1.spec
  Step2.spec
  Step3.spec
