module TestParticle (
    testParticle
) where

import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe)

import IdCtx (runIdCtx)
import Particle (newSpecies, newParticle, moveParticle, position, accelOn)
import Point (Point(..), diff, scale, normSq)

testParticle :: Spec
testParticle =
    let pointsWithinRadius :: Double -> Point -> Point -> Bool
        pointsWithinRadius rad p0 p1 = normSq (diff p0 p1) <= rad * rad
    in describe "Particle" $ do
        describe "(==) @Species" $ do
            it "should consider species with the same id to be equal" $
                let species = runIdCtx (newSpecies 5 5 5)
                in species `shouldBe` species
            it "should consider species with different ids to be different" $ runIdCtx $ do
                species0 <- newSpecies 5 5 5
                species1 <- newSpecies 5 5 5
                pure $ species0 `shouldNotBe` species1
        describe "(==) @Particle" $ do
            it "should consider particles with the same id to be equal" $
                let species = runIdCtx (newSpecies 5 5 5)
                    particle = runIdCtx (newParticle species (Point 5 5 5))
                in particle `shouldBe` particle
            it "should consider particles with different ids to be different" $ runIdCtx $ do
                let species = runIdCtx (newSpecies 5 5 5)
                particle0 <- newParticle species (Point 5 5 5)
                particle1 <- newParticle species (Point 5 5 5)
                pure $ particle0 `shouldNotBe` particle1
        describe "moveParticle" $ do
            it "should preserve id" $
                let species = runIdCtx (newSpecies 5 5 5)
                    particle = runIdCtx (newParticle species (Point 5 5 5))
                in moveParticle (Point 1 2 3) particle `shouldBe` particle
            it "should be a no-op if the bump dir is 0" $
                let species = runIdCtx (newSpecies 5 5 5)
                    particle = runIdCtx (newParticle species (Point 5 5 5))
                in position (moveParticle (Point 0 0 0) particle) `shouldBe` position particle
            it "should move the particle along the specified vector" $
                let species = runIdCtx (newSpecies 5 5 5)
                    particle = runIdCtx (newParticle species (Point 1 2 3))
                in position (moveParticle (Point 5 1 (-3)) particle) `shouldBe` Point 6 3 0
        describe "accelOn" $ do
            it "should return accel of repulsion when no affinity map" $ runIdCtx $ do
                let rad = 3
                    mass = 5
                    species = runIdCtx (newSpecies mass rad 5)
                particle0 <- newParticle species (Point 1 1 1) 
                particle1 <- newParticle species (Point 1 2 3)
                let expectedAccel = scale (8 * (rad ** 4) / 3 / mass * sqrt 5 ** (-4)) (Point 0 1 2)
                pure $ pointsWithinRadius 0.00000001 (particle0 `accelOn` particle1) expectedAccel
