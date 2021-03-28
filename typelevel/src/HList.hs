
{-# OPTIONS_GHC
    -Wno-unused-imports
#-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}

module HList where

import Data.Kind (Type, Constraint)
import Data.Singletons
import Data.Singletons.Prelude.Functor


-- This doesn't work, sadly
-- type Unconstrained :: Type -> Constraint
-- type Unconstrained a = ()

class Unconstrained a
instance Unconstrained a

-- | Heterogenous list with constraint
data HList :: (Type -> Constraint) -> [Type] -> Type where
    Nil     ::                              HList c '[]
    Cons    :: c a => a -> HList c xs ->    HList c (a ': xs)

infixr 5 `Cons`

hlist :: forall c ts a. (forall t. c t => t -> a) -> HList c ts -> [a]
hlist _ Nil = []
hlist f (Cons x xs) = f x : hlist f xs

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
    Could not deduce: c1 (Apply mt a) arising from a use of ‘Cons’

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
-- hlmap _ Nil = Nil
-- hlmap f (Cons x xs) = f x `Cons` hlmap f xs

demo :: HList Unconstrained '[Int, String, Bool]
demo = 5 `Cons` "Hello" `Cons` True `Cons` Nil

class SomethingFoldable a
instance Foldable f => SomethingFoldable (f a)

demo2 :: HList SomethingFoldable '[[Int]]
demo2 = Cons [] Nil
