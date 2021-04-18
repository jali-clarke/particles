module TestIdCtx (
    testIdCtx
) where

import Test.Hspec (Spec, describe, it, shouldBe)

import Control.Monad (replicateM)

import IdCtx (runIdCtx, getNextId)

testIdCtx :: Spec
testIdCtx = describe "id context" $ do
    it "should give 0 as the first id" $ runIdCtx getNextId `shouldBe` 0
    it "should give 1 as the second id" $ runIdCtx (getNextId *> getNextId) `shouldBe` 1
    it "should carry over the id source in a sequence" $ runIdCtx (replicateM 3 getNextId) `shouldBe` [0, 1, 2]
    it "should be an instance of Monad" $ runIdCtx (getNextId >>= pure) `shouldBe` 0
