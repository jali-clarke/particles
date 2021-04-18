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
    setAffinityMap
) where

import Data.IntMap (IntMap, empty, fromList)

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

setAffinityMap :: [(Species, Double)] -> Species -> Species
setAffinityMap protoMap thisSpecies =
    let getSpeciesIdFst (species', affinity) = (_speciesId species', affinity)
    in thisSpecies {_affinityMap = fromList (fmap getSpeciesIdFst protoMap)}
