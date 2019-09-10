module Hydra.Testing.Wrappers where

import           Test.Hspec (Spec, SpecWith, describe)

fastTest :: SpecWith () -> Spec
fastTest = describe "Fast"

slowTest :: SpecWith () -> Spec
slowTest = describe "Slow"

stableTest :: SpecWith a -> SpecWith a
stableTest = describe "Stable"

unstableTest :: SpecWith a -> SpecWith a
unstableTest = describe "Unstable"
