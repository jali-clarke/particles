module World (
    World,

    newWorld,
    allParticles,
    addParticle
) where

import Particle (Particle)

data World = World {_xMax :: Double, _yMax :: Double, _zMax :: Double, _allParticles :: [Particle]}

newWorld :: Double -> Double -> Double -> World
newWorld xMax yMax zMax = World xMax yMax zMax []

allParticles :: World -> [Particle]
allParticles = _allParticles

addParticle :: Particle -> World -> World
addParticle particle world = world {_allParticles = [particle]}
