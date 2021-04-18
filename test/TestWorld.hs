module TestWorld (
    testWorld
) where

import Test.Hspec (Spec, describe, it, shouldBe)

import IdCtx (runIdCtx)
import Particle (newSpecies, newParticle)
import World (newWorld, getNearbyParticles)

testWorld :: Spec
testWorld = describe "world actions" $ do
    describe "get nearby particles" $ do
        it "should return an empty list if there are no particles in the world" $
            let world = newWorld 10 10 10
                species = runIdCtx (newSpecies 0.5 0.75)
                particle = runIdCtx (newParticle species 5 5 5)
            in getNearbyParticles world particle `shouldBe` []
