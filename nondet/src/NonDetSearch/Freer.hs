
{-# OPTIONS_GHC
    -Wno-unused-imports
    -Wno-orphans
#-}

-- Based on: http://hackage.haskell.org/package/freer-simple
module NonDetSearch.Freer where

import Control.Monad (join, MonadPlus(..))
--import Control.Monad hiding (guard)
import Control.Monad.Freer
import qualified Control.Monad.Freer.NonDet as F

import NonDet.Class
import Debug.Trace

{-----------------------------------------------------------------------------}
-- Nondeterministic, visualizable search

type ND effs = Member F.NonDet effs

instance ND effs => NonDet (Eff effs) where
    failure :: Eff effs a
    failure     = mzero
    choice :: Eff effs a -> Eff effs a -> Eff effs a
    choice      = mplus

-- ARGH, mega-slow
searchFreer :: SFun
searchFreer = SFun search where
    search :: Eff '[F.NonDet] a -> [a]
    search m = run $ F.makeChoiceA m

{-----------------------------------------------------------------------------}

-- For profiling
profFreer :: [String] -> IO ()
profFreer _ = print $ getFun searchFreer (pidgeonHole' 8)
