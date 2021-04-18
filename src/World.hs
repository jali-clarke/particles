module World (
    World,

    newWorld,
    stepWorld,
    addParticle,

    allParticles,
    nearbyParticles
) where

import Point (Point(..), diff, normSq)
import Particle (Particle, neighbourhoodRadius, species, position)

data World = World {_topRight :: Point, _allParticles :: [Particle]}

newWorld :: Point -> World
newWorld topRight = World topRight []

stepWorld :: Double -> World -> World -> World
stepWorld _ _ world = world

addParticle :: Particle -> World -> World
addParticle particle world =
    let updatedSet = particle : _allParticles world
    in world {_allParticles = updatedSet}

allParticles :: World -> [Particle]
allParticles = _allParticles

nearbyParticles :: Particle -> World -> [Particle]
nearbyParticles particle world =
    let particlePosition = position particle
        particleIsNearby otherParticle =
            let distanceSquared = normSq $ diff particlePosition (position otherParticle)
                bounds = neighbourhoodRadius . species $ particle
            in distanceSquared <= bounds * bounds
    in filter particleIsNearby (_allParticles world)
