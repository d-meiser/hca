module Hca.EnsembleSpec (spec) where

import Test.Hspec
import Hca

spec :: Spec
spec = describe "Hca.Ensemble" $ do
  it "Has no particles when empty" $
    (numPtcls emptyEnsemble) `shouldBe` 0
