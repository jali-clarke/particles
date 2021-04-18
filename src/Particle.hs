module Particle (
    Species,
    Particle,

    newSpecies,
    newParticle,

    particleId,
    x,
    y,
    z,
    species,
    neighbourhoodRadius
) where

import IdCtx (IdCtx, getNextId)

data Species = Species {_speciesId :: Int, _mass :: Double, _radius :: Double, _neighbourhoodRadius :: Double} deriving Show
data Particle = Particle {_particleId :: Int, _species :: Species, _x :: Double, _y :: Double, _z :: Double} deriving Show

instance Eq Species where
    species0 == species1 = _speciesId species0 == _speciesId species1

instance Eq Particle where
    particle0 == particle1 = _particleId particle0 == _particleId particle1

newSpecies :: Double -> Double -> Double -> IdCtx Species
newSpecies mass radius thisNeighbourhoodRadius = fmap (\nextId -> Species nextId mass radius thisNeighbourhoodRadius) getNextId

newParticle :: Species -> Double -> Double -> Double -> IdCtx Particle
newParticle thisSpecies thisX thisY thisZ = fmap (\nextId -> Particle nextId thisSpecies thisX thisY thisZ) getNextId

particleId :: Particle -> Int
particleId = _particleId

x :: Particle -> Double
x = _x

y :: Particle -> Double
y = _y

z :: Particle -> Double
z = _z

species :: Particle -> Species
species = _species

neighbourhoodRadius :: Species -> Double
neighbourhoodRadius = _neighbourhoodRadius
