module Point (
    Point(..),

    diff,
    normSq
) where

data Point = Point {x :: Double, y :: Double, z :: Double} deriving (Eq, Show)

diff :: Point -> Point -> Point
diff (Point x0 y0 z0) (Point x1 y1 z1) = Point (x0 - x1) (y0 - y1) (z0 - z1)

normSq :: Point -> Double
normSq (Point thisX thisY thisZ) = thisX * thisX + thisY * thisY + thisZ * thisZ
