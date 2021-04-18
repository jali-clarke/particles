module TestWorld (
    testWorld
) where

import Test.Hspec (Spec, describe, it, shouldBe)

testWorld :: Spec
testWorld = describe "world actions" $
    it "should pass" $ 1 `shouldBe` (1 :: Int)
