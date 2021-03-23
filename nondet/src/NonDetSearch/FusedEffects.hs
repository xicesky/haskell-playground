
-- Source: https://hackage.haskell.org/package/fused-effects-1.1.1.0/docs/Control-Effect-NonDet.html
module NonDetSearch.FusedEffects where

import Data.Functor.Identity (Identity)
import Control.Effect.Empty (empty)
import Control.Effect.Choose ((<|>))
import Control.Effect.NonDet (oneOf)
-- import Control.Effect.Cut
-- import Control.Effect.Cull
import Control.Carrier.NonDet.Church (NonDetC, run, runNonDetA)

import qualified NonDet.Class as NDC

type Carrier = NonDetC Identity

{-----------------------------------------------------------------------------}
-- NonDet interface

instance NDC.NonDet Carrier where
    failure = empty
    choice = (<|>)
    anyof = oneOf

searchFE :: NDC.SFun
searchFE = NDC.SFun search where
    search :: Carrier a -> [a]
    search = run . runNonDetA
