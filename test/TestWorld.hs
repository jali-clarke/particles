module TestWorld (
    testWorld
) where

import Test.Hspec (Spec, describe, it, shouldBe)

import Control.Monad (replicateM)
import Data.List (sortOn)

import IdCtx (runIdCtx)
import Particle (newSpecies, newParticle, particleId)
import World (newWorld, addParticle, allParticles)

testWorld :: Spec
testWorld = describe "World" $ do
    describe "#allParticles" $ do
        it "should return empty list of particles if empty world" $ allParticles (newWorld 10 10 10) `shouldBe` []
        it "should return singleton list if only one in world" $
            let species = runIdCtx (newSpecies 0.1 0.1 0.2)
                particle = runIdCtx (newParticle species 5 5 5)
                world = newWorld 10 10 10
            in allParticles (addParticle particle world) `shouldBe` [particle]
        it "should return list of all inserted particles" $
            let species = runIdCtx (newSpecies 0.1 0.1 0.2)
                particles = runIdCtx (replicateM 5 $ newParticle species 5 5 5)
                world = foldr addParticle (newWorld 10 10 10) particles
            in sortOn particleId (allParticles world) `shouldBe` particles
