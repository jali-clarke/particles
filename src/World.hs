module World (
    World,

    newWorld,
    addParticle,

    allParticles,
    nearbyParticles
) where

import Particle (Particle)

data World = World {_xMax :: Double, _yMax :: Double, _zMax :: Double, _allParticles :: [Particle]}

newWorld :: Double -> Double -> Double -> World
newWorld xMax yMax zMax = World xMax yMax zMax []

addParticle :: Particle -> World -> World
addParticle particle world =
    let updatedSet = particle : _allParticles world
    in world {_allParticles = updatedSet}

allParticles :: World -> [Particle]
allParticles = _allParticles

nearbyParticles :: Particle -> World -> [Particle]
nearbyParticles _ world = if null (_allParticles world) then [] else [head $ _allParticles world]
