module Particle (
    Species,
    Particle,

    SpeciesId,
    ParticleId,

    newSpecies,
    newParticle,

    particleId,
    x,
    y,
    z,
    species,
    neighbourhoodRadius,
    setAffinityMap
) where

import Data.IntMap (IntMap, empty, fromList)

import IdCtx (IdCtx, getNextId)

type SpeciesId = Int
type ParticleId = Int

data Species = Species {
    _speciesId :: SpeciesId,
    _mass :: Double,
    _radius :: Double,
    _neighbourhoodRadius :: Double,
    _affinityMap :: IntMap Double
} deriving Show

data Particle = Particle {
    _particleId :: ParticleId,
    _species :: Species,
    _x :: Double,
    _y :: Double,
    _z :: Double
} deriving Show

instance Eq Species where
    species0 == species1 = _speciesId species0 == _speciesId species1

instance Eq Particle where
    particle0 == particle1 = _particleId particle0 == _particleId particle1

newSpecies :: Double -> Double -> Double -> IdCtx Species
newSpecies mass radius thisNeighbourhoodRadius = fmap (\nextId -> Species nextId mass radius thisNeighbourhoodRadius empty) getNextId

newParticle :: Species -> Double -> Double -> Double -> IdCtx Particle
newParticle thisSpecies thisX thisY thisZ = fmap (\nextId -> Particle nextId thisSpecies thisX thisY thisZ) getNextId

particleId :: Particle -> ParticleId
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

setAffinityMap :: [(Species, Double)] -> Species -> Species
setAffinityMap protoMap thisSpecies =
    let getSpeciesIdFst (species', affinity) = (_speciesId species', affinity)
    in thisSpecies {_affinityMap = fromList (fmap getSpeciesIdFst protoMap)}
