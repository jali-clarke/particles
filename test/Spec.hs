import Test.Hspec (hspec)

import TestIdCtx (testIdCtx)
import TestParticle (testParticle)
import TestWorld (testWorld)

main :: IO ()
main = hspec $ do
    testIdCtx
    testParticle
    testWorld
