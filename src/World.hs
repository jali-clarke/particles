module World (
    World,

    newWorld,
    stepWorld,
    addParticle,

    allParticles,
    getParticle,
    nearbyParticles
) where

import Data.IntMap (IntMap, (!), empty, toList, insert)

import Point (Point(..), diff, normSq)
import Particle (Particle, ParticleId, particleId, neighbourhoodRadius, species, position)

data World = World {_topRight :: Point, _allParticles :: IntMap Particle}

newWorld :: Point -> World
newWorld topRight = World topRight empty

stepWorld :: Double -> World -> World -> World
stepWorld _ _ world = world

addParticle :: Particle -> World -> World
addParticle particle world =
    let updatedSet = insert (particleId particle) particle (_allParticles world)
    in world {_allParticles = updatedSet}

allParticles :: World -> [Particle]
allParticles = fmap snd . toList . _allParticles

getParticle :: ParticleId -> World -> Particle
getParticle pid world = _allParticles world ! pid

nearbyParticles :: Particle -> World -> [Particle]
nearbyParticles particle world =
    let particlePosition = position particle
        particleIsNearby otherParticle =
            let distanceSquared = normSq $ diff particlePosition (position otherParticle)
                bounds = neighbourhoodRadius . species $ particle
            in distanceSquared <= bounds * bounds
    in filter particleIsNearby (allParticles world)
