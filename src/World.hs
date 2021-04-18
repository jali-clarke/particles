module World (
    World,

    newWorld,
    getNearbyParticles
) where

import Particle (Particle)

data World = World {_xMax :: Double, _yMax :: Double, _zMax :: Double}

newWorld :: Double -> Double -> Double -> World
newWorld = World

getNearbyParticles :: World -> Particle -> [Particle]
getNearbyParticles _ _ = []
