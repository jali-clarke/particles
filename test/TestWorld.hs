module TestWorld (
    testWorld
) where

import Test.Hspec (Spec, describe, it, xit, shouldBe, shouldSatisfy)

import Control.Monad (replicateM)
import Data.List (sortOn)

import IdCtx (runIdCtx)
import Particle (Particle, newSpecies, newParticle, moveParticle, particleId, position, updateAffinityMap)
import Point (Point(..), diff, normSq)
import World (newWorld, stepWorld, addParticle, allParticles, getParticle, nearbyParticles)

coordinatesEqual :: Particle -> Particle -> Bool
coordinatesEqual particle0 particle1 = position particle0 == position particle1

testWorld :: Spec
testWorld = describe "World" $ do
    describe "allParticles" $ do
        it "should return empty list of particles if empty world" $ allParticles (newWorld (Point 10 10 10)) `shouldBe` []
        it "should return singleton list if only one in world" $
            let species = runIdCtx (newSpecies 0.1 0.1 0.2)
                particle = runIdCtx (newParticle species (Point 5 5 5))
                world = newWorld (Point 10 10 10)
            in allParticles (addParticle particle world) `shouldBe` [particle]
        it "should return list of all inserted particles" $
            let species = runIdCtx (newSpecies 0.1 0.1 0.2)
                particles = runIdCtx (replicateM 5 $ newParticle species (Point 5 5 5))
                world = foldr addParticle (newWorld (Point 10 10 10)) particles
            in sortOn particleId (allParticles world) `shouldBe` particles
    describe "nearbyParticles" $ do
        it "should return empty list of particles if empty world" $
            let species = runIdCtx (newSpecies 0.1 0.1 0.2)
                particle = runIdCtx (newParticle species (Point 5 5 5))
                world = newWorld (Point 10 10 10)
            in nearbyParticles particle world `shouldBe` []
        it "should return singleton list of particles if sole particle is nearby" $ runIdCtx $ do
            let species = runIdCtx (newSpecies 0.1 0.1 5)
            particle0 <- newParticle species (Point 5 5 5)
            particle1 <- newParticle species (Point 5 6 7)
            let world = addParticle particle1 (newWorld (Point 10 10 10))
            pure $ nearbyParticles particle0 world `shouldBe` [particle1]
        it "should skip particle that is not nearby" $ runIdCtx $ do
            let species = runIdCtx (newSpecies 0.1 0.1 5)
            particle0 <- newParticle species (Point 5 5 5)
            particle1 <- newParticle species (Point 5 6 7)
            particle2 <- newParticle species (Point 10 10 10)
            let world = foldr addParticle (newWorld (Point 10 10 10)) [particle1, particle2]
            pure $ nearbyParticles particle0 world `shouldBe` [particle1]
        it "should skip multiple particles that are not nearby and keep ones that are" $ runIdCtx $ do
            let species = runIdCtx (newSpecies 0.1 0.1 2)    
            particle0 <- newParticle species (Point 5 5 5)
            particle1 <- newParticle species (Point 5 6 6)
            particle2 <- newParticle species (Point 10 10 10)
            particle3 <- newParticle species (Point 4 4 4)
            particle4 <- newParticle species (Point 2.9 5 5)
            let world = foldr addParticle (newWorld (Point 10 10 10)) [particle1, particle2, particle3, particle4]
            pure $ sortOn particleId (nearbyParticles particle0 world) `shouldBe` [particle1, particle3]
    describe "getParticle" $ do
        it "should return a particle given its id" $ runIdCtx $ do
            let species = runIdCtx (newSpecies 0.1 0.1 5)
            particle0 <- newParticle species (Point 5 5 5)
            particle1 <- newParticle species (Point 5 6 7)
            let world = foldr addParticle (newWorld (Point 10 10 10)) [particle0, particle1]
            pure $ do
                getParticle (particleId particle0) world `shouldBe` particle0
                getParticle (particleId particle1) world `shouldBe` particle1
    describe "stepWorld" $ do
        it "should do nothing if there are no particles" $
            let world = newWorld (Point 10 10 10)
                nextWorld = stepWorld 0.01 world world
            in allParticles nextWorld `shouldBe` []
        it "should do nothing if all of the particles are too far away from each other" $ runIdCtx $ do
            let (smallSpecies', largeSpecies') = runIdCtx $ (,) <$> newSpecies 0.001 0.1 1 <*> newSpecies 0.001 0.5 4
                smallSpecies = foldr updateAffinityMap smallSpecies' [(largeSpecies', 100000), (smallSpecies', (-1000000))]
                largeSpecies = foldr updateAffinityMap largeSpecies' [(smallSpecies', 100000), (largeSpecies', (-1000000))]
            top <- newParticle largeSpecies (Point 0 10 0)
            bottom <- newParticle largeSpecies (Point 0 (-10) 0)
            left <- newParticle smallSpecies (Point (-10) 0 0)
            right <- newParticle smallSpecies (Point 10 0 0)
            let particles = [top, bottom, left, right]
                world = foldr addParticle (newWorld (Point 20 20 20)) particles
                nextWorld = stepWorld 0.01 world world
                allNextParticles = sortOn particleId (allParticles nextWorld)
            pure $ do
                allNextParticles `shouldBe` particles
                foldr (&&) True (zipWith coordinatesEqual allNextParticles particles) `shouldBe` True
        xit "should preserve velocity for non-interacting particles" $ runIdCtx $ do
            let (smallSpecies, largeSpecies) = runIdCtx $ (,) <$> newSpecies 0.001 0.1 1 <*> newSpecies 0.001 0.2 4
                leftInitPos = Point 1 0 0
                topInitPos = Point 1 0 1
                leftDiff = Point 0.5 0 0
                topDiff = Point 0 (-0.5) (-0.5)
            left <- newParticle smallSpecies leftInitPos
            top <- newParticle largeSpecies topInitPos
            let world = newWorld (Point 10 10 10)
                worldInit = foldr addParticle world [left, top]
                worldPrev = foldr addParticle world [moveParticle leftDiff left, moveParticle topDiff top]
                nextWorld = stepWorld 0.01 worldPrev worldInit
                allNextParticles = sortOn particleId (allParticles nextWorld)
            pure $ do
                normSq (position (allNextParticles !! 0) `diff` Point 1.5 0 0) `shouldSatisfy` (< 0.000001)
                normSq (position (allNextParticles !! 1) `diff` Point 1 (-0.5) 0.5) `shouldSatisfy` (< 0.000001)
