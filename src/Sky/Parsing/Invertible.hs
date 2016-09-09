
{-# LANGUAGE InstanceSigs #-}               -- Because i love it
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}                 -- Required to use SemiIso apparently

module Sky.Parsing.Invertible where

import Prelude hiding (id, (.))
import Control.Category                     -- yay

import Data.Functor.Identity
import Text.Read (readMaybe)

import Sky.Isomorphism.Class
import Sky.Isomorphism.SemiIso

-- Doesn't work with raw SemiIso: f (SemiIso' ...) requires ImpredicativeTypes
-- For now, go just with the mumu version
type Iso a b = MumuIso Maybe a a b b

infixl 4 <$>

class IsoFunctor f where
    (<$>) :: forall a b. Iso a b -> f a -> f b

infixl 4 <*>

class IsoFunctor f => IsoApplicative f where
    (<*>) :: forall a b. f (Iso a b) -> f a -> f b

-- Simple parser / pp from read / show
mkInvertible :: forall a. (Read a, Show a) => Iso String a
mkInvertible = iso readMaybe (return . show)
