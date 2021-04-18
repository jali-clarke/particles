module Particle (
    Species,
    Particle,

    newSpecies,
    newParticle
) where

data Species = Species {_speciesId :: Int, _mass :: Double, _radius :: Double} deriving (Eq, Show)
data Particle = Particle {_particleId :: Int, _species :: Species, _x :: Double, _y :: Double, _z :: Double} deriving (Eq, Show)

newSpecies :: Double -> Double -> Species
newSpecies = Species 0

newParticle :: Species -> Double -> Double -> Double -> Particle
newParticle = Particle 0
