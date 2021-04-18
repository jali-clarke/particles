import Test.Hspec (hspec)

import TestIdCtx (testIdCtx)
import TestWorld (testWorld)

main :: IO ()
main = hspec $ do
    testWorld
    testIdCtx
