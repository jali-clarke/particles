module Particle (
    Species,
    Particle,

    newSpecies,
    newParticle
) where

data Species = Species {_mass :: Double, _radius :: Double, _charge :: Double} deriving (Eq, Show)
data Particle = Particle {_species :: Species, _x :: Double, _y :: Double, _z :: Double} deriving (Eq, Show)

newSpecies :: Double -> Double -> Double -> Species
newSpecies = Species

newParticle :: Species -> Double -> Double -> Double -> Particle
newParticle = Particle
