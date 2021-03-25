
-- Based on: http://hackage.haskell.org/package/polysemy
module NonDetSearch.Polysemy where

import Polysemy
import Polysemy.NonDet

import NonDet.Class (SFun(..))
import qualified NonDet.Class as C

{-----------------------------------------------------------------------------}

{- 
Just like Freer, polysemy doen't supply a carrier, but uses a nice one
(NonDetC) internally: 
http://hackage.haskell.org/package/polysemy-1.4.0.0/docs/src/Polysemy.NonDet.html#

FIXME: There seem to be semantic problems with several NonDet impls, which
we /need to test/ for:
https://github.com/polysemy-research/polysemy/issues/246
-}

-- Sem (NonDet ': r) a -> Sem r (f a)

instance Member NonDet r => C.NonDet (Sem r)

searchPolysemy :: SFun
searchPolysemy = SFun search where
    search :: Sem '[NonDet] a -> [a]
    search m = run $ runNonDet m
