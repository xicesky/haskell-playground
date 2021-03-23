
-- Source: https://hackage.haskell.org/package/nondeterminism-1.4/docs/Control-Monad-Amb.html
module NonDetSearch.Amb where

import Control.Monad.Amb
import NonDet.Class

{-----------------------------------------------------------------------------}
-- NonDet interface

instance NonDet (AmbT r m) where
    anyof = aMemberOf

searchAmb :: SFun
searchAmb = SFun search where
    search :: Amb a a -> [a]
    search = allValues
