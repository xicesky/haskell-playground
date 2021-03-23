
-- This module uses uninitialized structs on purpose
{-# OPTIONS_GHC -Wno-missing-fields #-}

{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE DeriveDataTypeable     #-}

module Old.Sky.Isomorphism.ConstrIso where

import Prelude hiding ((.))
import Data.Data (Data, Constr, toConstr)

--import Sky.Classes.Isomorphism.Monomorphic
--import Sky.Implementations.Isomorphism
--import Sky.Implementations.Isomorphism.MonoIso

-- For definition of our Iso only
import Control.Category (Category, (.))
import qualified Control.Category as Cat
import Data.Functor.Identity
import Control.Monad ((<=<))
import Data.Tuple (swap)
import qualified Data.List as List

-- We can't use Data.Set, because Constr has no Ord instance
type PseudoSet a = [a]

-- "HACK": We use the empty list to represent "unknown" or "arbitrary" constructor sets
contains :: Eq a => PseudoSet a -> a -> Bool
contains [] _ = True
contains xs v = elem v xs

isDisjunct :: Eq a => PseudoSet a -> PseudoSet a -> Bool
isDisjunct [] _     = False
isDisjunct xs []    = False
isDisjunct xs ys    = null (List.intersect xs ys)

isEqual :: Eq a => PseudoSet a -> PseudoSet a -> Bool
isEqual [] []       = True
isEqual (x:xs) []   = False
isEqual [] (y:ys)   = False
isEqual xs ys       = null $ (List.union xs ys) List.\\ (List.intersect xs ys)

-- Definition of our iso
data PartialSemiIso m s a = PartialSemiIso
    { _rawPartialSemiIsoIso :: (s -> m a, a -> m s)
    , _partialDomain :: PseudoSet Constr
    , _partialCodomain :: PseudoSet Constr
    }

type Iso a b = PartialSemiIso Identity a b

-- instance PolymorphicSemiIsomorphism PackedSemiIso where
--     packPolymorphicSemiIsomorphism :: Monad m => (s -> m a, a' -> m s') -> PackedSemiIso m s s' a a'
--     packPolymorphicSemiIsomorphism = PackedSemiIso
--     unpackPolymorphicSemiIsomorphism :: Monad m => PackedSemiIso m s s' a a' -> (s -> m a, a' -> m s')
--     unpackPolymorphicSemiIsomorphism = _rawPackedSemiIso

_partialIsoM :: forall m s a. (Monad m) => PseudoSet Constr -> PseudoSet Constr -> (s -> m a) -> (a -> m s) -> PartialSemiIso m s a
_partialIsoM domain codomain forward backward = PartialSemiIso (forward, backward) domain codomain

_partialIso :: forall m s a. (Monad m) => PseudoSet Constr -> PseudoSet Constr -> (s -> a) -> (a -> s) -> PartialSemiIso m s a
_partialIso domain codomain forward backward = PartialSemiIso (return . forward, return . backward) domain codomain

partialIso :: forall m s a. (Monad m, Data s) => s -> (s -> a) -> (a -> s) -> PartialSemiIso m s a
partialIso proxy forward backward = _partialIso [toConstr proxy] [] forward backward

isoM :: forall m s a. (Monad m) => (s -> m a) -> (a -> m s) -> PartialSemiIso m s a
isoM forward backward = _partialIsoM [] [] forward backward

iso :: forall m s a. (Monad m) => (s -> a) -> (a -> s) -> PartialSemiIso m s a
iso forward backward = _partialIso [] [] forward backward

apply :: forall s a. Iso s a -> s -> a
apply iso = runIdentity . (fst $ _rawPartialSemiIsoIso iso)

unapply :: forall s a. Iso s a -> a -> s
unapply iso = runIdentity . (snd $ _rawPartialSemiIsoIso iso)

reverseIso :: forall s a. Iso s a -> Iso a s
reverseIso (PartialSemiIso fns domain codomain) = PartialSemiIso (swap fns) codomain domain

instance (Monad m) => Category (PartialSemiIso m) where
    id :: PartialSemiIso m a a
    id = iso id id

    (.) :: PartialSemiIso m b c -> PartialSemiIso m a b -> PartialSemiIso m a c
    (.) !isoBC !isoAB = if check then isoAC else err where
        check = isEqual (_partialCodomain isoAB) (_partialDomain isoBC)
        err = error $ "Iso seq domain mismatch: " ++ show (_partialCodomain isoAB) ++ " vs " ++ show (_partialDomain isoBC)
        (bmc, cmb) = _rawPartialSemiIsoIso isoBC
        (amb, bma) = _rawPartialSemiIsoIso isoAB
        amc = bmc <=< amb
        cma = bma <=< cmb
        isoAC = PartialSemiIso (amc, cma) (_partialDomain isoAB) (_partialCodomain isoBC)

data Example
    = Example1
    | Example2 Int
    | Example3 Int Int
    --  ...
    deriving (Data)

c_example1 :: Constr
c_example1 = toConstr $ Example1 {}
c_example2 :: Constr
c_example2 = toConstr $ Example2 {}
c_example3 :: Constr
c_example3 = toConstr $ Example3 {}

example1 :: Iso Example ()
example1 = partialIso (Example1 {}) forward backward where
    forward (Example1)  = ()
    forward _           = error $ "Partial iso"
    backward ()         = Example1

example2 :: Iso Example Int
example2 = partialIso (Example2 {}) forward backward where
    forward (Example2 i) = i
    forward _           = error $ "Partial iso"
    backward i          = Example2 i

example3 :: Iso Example (Int, Int)
example3 = partialIso (Example3 {}) forward backward where
    forward (Example3 i j) = (i, j)
    forward _           = error $ "Partial iso"
    backward (i, j)     = Example3 i j

eval :: a -> String
eval !a = "Ok."

isoConst :: a -> b -> Iso a b
isoConst a b = iso (const b) (const a)

isoCons :: Iso (a, [a]) [a]
isoCons = iso (\(x,xs) -> x:xs) (\(x:xs) -> (x,xs))

isoFixedRight :: b -> Iso (a,b) a
isoFixedRight fixed = iso (\(a,b) -> a) (\a -> (a, fixed))

isoFixedLeft :: a -> Iso (a,b) b
isoFixedLeft fixed = iso (\(a,b) -> b) (\b -> (fixed, b))

--isoSwap :: Iso (Either a b) (Either b a)
--isoRebalance :: Iso (Either (Either a b) c) (Either a (Either b c))

--isoSum :: Iso a b -> Iso c d -> Iso (Either a b) (Either c d)
--endoIso :: Iso a b -> Iso (Either a b) (Either a b)

-- "Functors preserve isomorphisms"
--isoFun :: Functor f => Iso a b -> Iso (f a) (f b)

isoSum :: forall m a b1 b2. (Monad m, Data a) => PartialSemiIso m a b1 -> PartialSemiIso m a b2 -> PartialSemiIso m a (Either b1 b2)
isoSum iso1 iso2 = if check then PartialSemiIso (forward, backward) newDomain []  else err where
    check = isDisjunct (_partialDomain iso1) (_partialDomain iso2)
    err = error $ "Iso sum domain clash: " ++ show (_partialDomain iso1) ++ " vs " ++ show (_partialDomain iso2)
    newDomain = List.union (_partialDomain iso1) (_partialDomain iso2)
    (a_mb1, b1_ma) = _rawPartialSemiIsoIso iso1
    (a_mb2, b2_ma) = _rawPartialSemiIsoIso iso2
    forward :: a -> m (Either b1 b2)
    forward a = if _partialDomain iso1 `contains` toConstr a
        then Left <$> a_mb1 a
        else Right <$> a_mb2 a
    backward :: Either b1 b2 -> m a
    backward (Left b1)                              = b1_ma b1
    backward (Right b2)                             = b2_ma b2
