
{-# OPTIONS_GHC
    -Wno-unused-imports
#-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}

module HList where

import Data.Kind (Type, Constraint)
import Data.Monoid
--import GHC.TypeLits
import Data.Singletons
import Data.Singletons.TypeLits
import Data.Singletons.Prelude.Num
import Data.Singletons.Prelude.List
import Data.Singletons.Prelude.Functor

{--------------------------------------------------------------------------------------------------}

-- This doesn't work, sadly
-- type Unconstrained :: Type -> Constraint
-- type Unconstrained a = ()

class Unconstrained a where
instance Unconstrained a where

{--------------------------------------------------------------------------------------------------}

-- | Heterogenous list with constraint
data HList :: (Type -> Constraint) -> [Type] -> Type where
    HNil    ::                              HList c '[]
    HCons   :: c a => a -> HList c xs ->    HList c (a ': xs)

infixr 5 `HCons`

hFoldMap :: Monoid m => (forall a. c a => a -> m) -> HList c ts -> m
hFoldMap _ HNil = mempty
hFoldMap f (HCons x xs) = f x `mappend` hFoldMap f xs

hFoldr :: (forall a. c a => a -> b -> b) -> b -> HList c ts -> b
hFoldr f z t = appEndo (hFoldMap (Endo . f) t) z

hFoldl' :: forall b c ts. (forall a. c a => b -> a -> b) -> b -> HList c ts -> b
hFoldl' f z0 xs = hFoldr f' id xs z0
    where
    f' :: forall a. c a => a -> (b -> b) -> (b -> b)
    f' x k z = k $! f z x

{--------------------------------------------------------------------------------------------------}

hHead :: c x => HList c (x ': xs) -> x
hHead (HCons a _) = a

hTail :: HList c (x ': xs) -> HList c xs
hTail (HCons _ as) = as

hToList :: forall c ts a. (forall t. c t => t -> a) -> HList c ts -> [a]
hToList f = hFoldMap (pure . f)

-- -- FIXME: Singletons probably already has something like this
-- data C c = forall a. C { unC :: c a => a }

-- hToListC :: HList c ts -> [C c]
-- hToListC = hToList C

-- ??
-- hTraverse :: Applicative f => (a -> f b) -> HList c ts -> f [b]
-- hTraverse = _
-- hSequenceA :: Applicative f => HList c (Fmap f ts) -> f (HList c ts)
-- hSequenceA = _

-- | Length, at compile time
type family HLength t :: Nat where
    HLength (HList c ts)        = Length ts

hLength :: HList c ts -> Int
hLength = hFoldl' (\c _ -> c + 1) 0

{--------------------------------------------------------------------------------------------------}

class Num (Mymap a) => SizeT a where
    type Mymap a :: Type
    showT :: Proxy a -> Mymap a

instance SizeT Int where
    type Mymap Int = Int
    showT _ = 1

-- class All (c :: Type -> Constraint) (m :: Type -> Type) (ts :: [Type])
-- instance All Num Mymap '[]

-- FoldMap for constraints :)
-- type family AllC (c :: Type -> Constraint) (ts :: [Type]) :: Constraint where
--     AllC c '[]       = ()
--     AllC c (x ': xs) = (c x, AllC c xs)

class AllC (c :: Type -> Constraint) (ts :: [Type]) where

instance AllC c '[]
instance (c x, AllC c xs) => AllC c (x ': xs)

class AllCM (c :: Type -> Constraint) (mt :: Type ~> Type) (ts :: [Type]) where

instance AllCM c mt '[]
instance (c (Apply mt x), AllCM c mt xs) => AllCM c mt (x ': xs)

{- | HList is a functor for polymorphic functions if we specify the mapping...
But i don't know how to guarantee that @mt@ always maps to correctly constrained
variables:
    Could not deduce: c1 (Apply mt a) arising from a use of ‘HCons’

Using QuantifiedConstraints i can even write:
    (forall a. c1 (Apply mt a)) => ...

GHC doesn't really like @mt@ either and introduces another mt0 which
ends up being ambiguous (i don't get it).

-}
-- hlmap :: forall
--     (c :: Type -> Constraint) (c1 :: Type -> Constraint)
--     (ts :: [Type]) (ts1 :: [Type])
--     (mt :: Type ~> Type).                   -- Mapping for the types

--     (   ts1 ~ Fmap mt ts
--     ,   forall a. c1 (Apply mt a)
--     ) =>

--     (forall t. (c t) => t -> Apply mt t) -- Actual poly function
--     -> HList c ts -> HList c1 ts1
-- hlmap _ HNil = HNil
-- hlmap f (HCons x xs) = f x `HCons` hlmap f xs

demo :: HList Unconstrained '[Int, String, Bool]
demo = 5 `HCons` "Hello" `HCons` True `HCons` HNil

class SomethingFoldable a
instance Foldable f => SomethingFoldable (f a)

demo2 :: HList SomethingFoldable '[[Int]]
demo2 = HCons [] HNil
