module TestIdCtx (
    testIdCtx
) where

import Test.Hspec (Spec, describe, it, shouldBe)

import IdCtx (runIdCtx, getNextId)

testIdCtx :: Spec
testIdCtx = describe "id context" $ do
    it "should give 0 as the first id" $ runIdCtx getNextId `shouldBe` 0
