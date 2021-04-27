module Particle (
    Species,
    Particle,

    SpeciesId,
    ParticleId,

    newSpecies,
    newParticle,
    moveParticle,

    particleId,
    position,
    species,
    neighbourhoodRadius,
    accelOn,
    updateAffinityMap
) where

import Data.IntMap (IntMap, (!?), empty, insert)

import IdCtx (IdCtx, getNextId)
import Point (Point(..), add, diff, scale, normSq)

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

moveParticle :: Point -> Particle -> Particle
moveParticle displacement particle =
    let oldPosition = _position particle
    in particle {_position = add displacement oldPosition}

particleId :: Particle -> ParticleId
particleId = _particleId

position :: Particle -> Point
position = _position

species :: Particle -> Species
species = _species

neighbourhoodRadius :: Species -> Double
neighbourhoodRadius = _neighbourhoodRadius

accelOn :: Particle -> Particle -> Point
accelOn (Particle _ species0 point0) (Particle _ species1 point1) =
    let Species speciesId0 _ radius0 _ affinityMap0 = species0
        affinity =
            case affinityMap0 !? speciesId0 of
                Nothing -> 0
                Just affinity0 -> affinity0
        mass1 = _mass species1
        dirVec = diff point1 point0
        distSq = normSq dirVec
        repulsionForceCoeff = 8 * radius0 ^ (4 :: Int) / 3
    in scale ((repulsionForceCoeff / (distSq ^ (2 :: Int)) - affinity / (distSq * sqrt distSq)) / mass1) dirVec

updateAffinityMap :: (Species, Double) -> Species -> Species
updateAffinityMap (otherSpecies, affinity) thisSpecies =
    let oldMap = _affinityMap thisSpecies
    in thisSpecies {_affinityMap = insert (_speciesId otherSpecies) affinity oldMap}
