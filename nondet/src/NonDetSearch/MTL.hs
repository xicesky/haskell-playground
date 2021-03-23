
-- Use mtl monad transformers
module NonDetSearch.MTL where

import Control.Applicative
import Control.Monad.Cont
import NonDet.Class

type NonDetM r = ContT r []

instance Alternative (NonDetM r) where
    empty = lift empty
    -- (<|>) :: CPS n a -> CPS n a -> CPS n a
    -- (<|>) a b = CPS $ \cont ->
    --     (<|>) (a `unCPS` cont) (b `unCPS` cont)

    (<|>) (ContT l) (ContT r) = ContT $ \c ->
        l c <|> r c

instance MonadPlus (NonDetM r)
instance NonDet (NonDetM r)

runNonDetM :: forall a. (forall r. NonDetM r a) -> [a]
runNonDetM m = runContT m pure

searchMTL :: SFun
searchMTL = SFun runNonDetM
