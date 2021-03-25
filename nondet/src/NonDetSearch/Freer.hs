
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
--import Debug.Trace

{-----------------------------------------------------------------------------}
-- Nondeterministic, visualizable search

instance Member F.NonDet effs => NonDet (Eff effs) where
    failure :: Eff effs a
    failure     = mzero
    choice :: Eff effs a -> Eff effs a -> Eff effs a
    choice      = mplus

searchFreer :: SFun
searchFreer = SFun search where
    search :: Eff '[F.NonDet] a -> [a]
    search m = run $ F.makeChoiceA m

{- Using msplit doesn't help -}
-- searchFreer :: SFun
-- searchFreer = SFun search where
--     search :: Eff '[F.NonDet] a -> [a]
--     search m = run $ loop m     -- Not supported
--     loop :: Eff '[F.NonDet] a -> Eff '[F.NonDet] [a]
--     loop m = F.msplit m >>= \case
--         Nothing -> return []
--         Just (a, rest) -> (a :) <$> loop rest

{-----------------------------------------------------------------------------}

-- For profiling
profFreer :: [String] -> IO ()
profFreer _ = print $ getFun searchFreer (pidgeonHole' 8)
