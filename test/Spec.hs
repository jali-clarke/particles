import Test.Hspec (hspec)

import TestWorld (testWorld)

main :: IO ()
main = hspec $ do
    testWorld
