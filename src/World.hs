module World (
    World,

    newWorld,
    stepWorld,
    addParticle,

    allParticles,
    nearbyParticles
) where

import Particle (Particle, neighbourhoodRadius, species, x, y, z)

data World = World {_xMax :: Double, _yMax :: Double, _zMax :: Double, _allParticles :: [Particle]}

newWorld :: Double -> Double -> Double -> World
newWorld xMax yMax zMax = World xMax yMax zMax []

stepWorld :: Double -> World -> World
stepWorld _ world = world

addParticle :: Particle -> World -> World
addParticle particle world =
    let updatedSet = particle : _allParticles world
    in world {_allParticles = updatedSet}

allParticles :: World -> [Particle]
allParticles = _allParticles

nearbyParticles :: Particle -> World -> [Particle]
nearbyParticles particle world =
    let particleIsNearby otherParticle =
            let square = (^ (2 :: Int))
                squaredDiffCoord coord = square (coord particle - coord otherParticle)
                squaredDist = squaredDiffCoord x + squaredDiffCoord y + squaredDiffCoord z
            in squaredDist <= square (neighbourhoodRadius . species $ particle)
    in filter particleIsNearby (_allParticles world)
