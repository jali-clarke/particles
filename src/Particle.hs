module Particle (
    Species,
    Particle,

    SpeciesId,
    ParticleId,

    newSpecies,
    newParticle,

    particleId,
    position,
    species,
    neighbourhoodRadius,
    updateAffinityMap
) where

import Data.IntMap (IntMap, empty, insert)

import IdCtx (IdCtx, getNextId)
import Point (Point(..))

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
    _position :: Point
} deriving Show

instance Eq Species where
    species0 == species1 = _speciesId species0 == _speciesId species1

instance Eq Particle where
    particle0 == particle1 = _particleId particle0 == _particleId particle1

newSpecies :: Double -> Double -> Double -> IdCtx Species
newSpecies mass radius thisNeighbourhoodRadius = fmap (\nextId -> Species nextId mass radius thisNeighbourhoodRadius empty) getNextId

newParticle :: Species -> Point -> IdCtx Particle
newParticle thisSpecies thisPosition = fmap (\nextId -> Particle nextId thisSpecies thisPosition) getNextId

particleId :: Particle -> ParticleId
particleId = _particleId

position :: Particle -> Point
position = _position

species :: Particle -> Species
species = _species

neighbourhoodRadius :: Species -> Double
neighbourhoodRadius = _neighbourhoodRadius

updateAffinityMap :: (Species, Double) -> Species -> Species
updateAffinityMap (otherSpecies, affinity) thisSpecies =
    let oldMap = _affinityMap thisSpecies
    in thisSpecies {_affinityMap = insert (_speciesId otherSpecies) affinity oldMap}
