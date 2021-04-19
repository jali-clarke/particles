module TestParticle (
    testParticle
) where

import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe)

import IdCtx (runIdCtx)
import Particle (newSpecies, newParticle, moveParticle, position)
import Point (Point(..))

testParticle :: Spec
testParticle = describe "Particle" $ do
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