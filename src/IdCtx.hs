module IdCtx (
    IdCtx,

    runIdCtx,
    getNextId
) where

data IdCtx a = IdCtx a

runIdCtx :: IdCtx a -> a
runIdCtx (IdCtx thing) = thing

getNextId :: IdCtx Int
getNextId = IdCtx 0
