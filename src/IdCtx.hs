{-# LANGUAGE
    GeneralizedNewtypeDeriving
#-}

module IdCtx (
    IdCtx,

    runIdCtx,
    getNextId
) where

import Control.Monad.State (State, evalState, state)

newtype IdCtx a = IdCtx (State Int a) deriving (Functor, Applicative, Monad)

runIdCtx :: IdCtx a -> a
runIdCtx (IdCtx stateAction) = evalState stateAction 0

getNextId :: IdCtx Int
getNextId = IdCtx (state (\nextId -> (nextId, nextId + 1)))
