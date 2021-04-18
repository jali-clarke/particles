module TestWorld (
    testWorld
) where

import Test.Hspec (Spec, describe, it, shouldBe)

import Control.Monad (replicateM)
import Data.List (sortOn)

import IdCtx (runIdCtx)
import Particle (newSpecies, newParticle, particleId)
import World (newWorld, stepWorld, addParticle, allParticles, nearbyParticles)

testWorld :: Spec
testWorld = describe "World" $ do
    describe "allParticles" $ do
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
    describe "nearbyParticles" $ do
        it "should return empty list of particles if empty world" $
            let species = runIdCtx (newSpecies 0.1 0.1 0.2)
                particle = runIdCtx (newParticle species 5 5 5)
                world = newWorld 10 10 10
            in nearbyParticles particle world `shouldBe` []
        it "should return singleton list of particles if sole particle is nearby" $ runIdCtx $ do
            let species = runIdCtx (newSpecies 0.1 0.1 5)
            particle0 <- newParticle species 5 5 5
            particle1 <- newParticle species 5 6 7
            let world = addParticle particle1 (newWorld 10 10 10)
            pure $ nearbyParticles particle0 world `shouldBe` [particle1]
        it "should skip particle that is not nearby" $ runIdCtx $ do
            let species = runIdCtx (newSpecies 0.1 0.1 5)
            particle0 <- newParticle species 5 5 5
            particle1 <- newParticle species 5 6 7
            particle2 <- newParticle species 10 10 10
            let world = foldr addParticle (newWorld 10 10 10) [particle1, particle2]
            pure $ nearbyParticles particle0 world `shouldBe` [particle1]
        it "should skip multiple particles that are not nearby and keep ones that are" $ runIdCtx $ do
            let species = runIdCtx (newSpecies 0.1 0.1 2)    
            particle0 <- newParticle species 5 5 5
            particle1 <- newParticle species 5 6 6
            particle2 <- newParticle species 10 10 10
            particle3 <- newParticle species 4 4 4
            particle4 <- newParticle species 2.9 5 5
            let world = foldr addParticle (newWorld 10 10 10) [particle1, particle2, particle3, particle4]
            pure $ sortOn particleId (nearbyParticles particle0 world) `shouldBe` [particle1, particle3]
    describe "stepWorld" $ do
        it "should do nothing there are no particles" $
            let world = newWorld 10 10 10
                nextWorld = stepWorld world
            in allParticles nextWorld `shouldBe` []
