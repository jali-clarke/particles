module TestWorld (
    testWorld
) where

import Test.Hspec (Spec, describe, it, shouldBe)

import Particle (newSpecies, newParticle)
import World (newWorld, getNearbyParticles)

testWorld :: Spec
testWorld = describe "world actions" $ do
    describe "get nearby particles" $ do
        it "should return an empty list if there are no particles in the world" $
            let world = newWorld 10 10 10
                species = newSpecies 0.5 0.75 1
                particle = newParticle species 5 5 5
            in getNearbyParticles world particle `shouldBe` []
