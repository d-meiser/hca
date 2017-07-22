module Hca.EnsembleSpec (spec) where

import Test.Hspec
import Hca

atOrigin :: Int -> Vec3
atOrigin _ = Vec3 0.0 0.0 0.0

spec :: Spec
spec = describe "Hca.Ensemble" $ do
  it "Has no particles when empty" $
    (numPtcls emptyEnsemble) `shouldBe` 0
  it "Has right number of particles" $
    (numPtcls $ generateParticles 10 atOrigin atOrigin) `shouldSatisfy` (== (10::Int))
