module Particle (
    Species,
    Particle,

    newSpecies,
    newParticle
) where

import IdCtx (IdCtx, getNextId)

data Species = Species {_speciesId :: Int, _mass :: Double, _radius :: Double, _neighbourhoodRadius :: Double} deriving Show
data Particle = Particle {_particleId :: Int, _species :: Species, _x :: Double, _y :: Double, _z :: Double} deriving Show

instance Eq Species where
    species0 == species1 = _speciesId species0 == _speciesId species1

instance Eq Particle where
    particle0 == particle1 = _particleId particle0 == _particleId particle1

newSpecies :: Double -> Double -> Double -> IdCtx Species
newSpecies mass radius neighbourhoodRadius = fmap (\nextId -> Species nextId mass radius neighbourhoodRadius) getNextId

newParticle :: Species -> Double -> Double -> Double -> IdCtx Particle
newParticle species x y z = fmap (\nextId -> Particle nextId species x y z) getNextId
