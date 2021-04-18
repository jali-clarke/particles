module Point (
    Point(..),

    add,
    diff,
    scale,
    normSq
) where

data Point = Point {x :: Double, y :: Double, z :: Double} deriving (Eq, Show)

add :: Point -> Point -> Point
add (Point x0 y0 z0) (Point x1 y1 z1) = Point (x0 + x1) (y0 + y1) (z0 + z1)

diff :: Point -> Point -> Point
diff (Point x0 y0 z0) (Point x1 y1 z1) = Point (x0 - x1) (y0 - y1) (z0 - z1)

scale :: Double -> Point -> Point
scale c (Point x' y' z') = Point (c * x') (c * y') (c * z')

normSq :: Point -> Double
normSq (Point x' y' z') = x' * x' + y' * y' + z' * z'
