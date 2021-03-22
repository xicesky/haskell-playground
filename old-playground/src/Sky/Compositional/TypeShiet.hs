
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveTraversable      #-}

{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE DataKinds              #-}

{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE TypeOperators          #-}

-- {-# LANGUAGE TypeOperators          #-}
-- {-# LANGUAGE MultiParamTypeClasses  #-}
-- {-# LANGUAGE UndecidableInstances   #-}

-- | Type-level programming (GHC8)
module Sky.Compositional.TypeShiet where

-- https://downloads.haskell.org/~ghc/8.0.1/docs/html/users_guide/glasgow_exts.html#kind-polymorphism-and-type-in-type
-- https://downloads.haskell.org/~ghc/8.0.1/docs/html/users_guide/glasgow_exts.html#type-level-literals

import GHC.TypeLits
import Data.Proxy
import Data.Kind (Type)

-- data TList a = TNil | TCons a (TList a)

-- type family IsNull l where
--     IsNull TNil         = True
--     IsNull (TCons _ _)  = False

-- type family TLength l where
--     TLength TNil        = 0
--     TLength (TCons _ a) = (TLength a) + 1

type family MLength l :: Nat where
    MLength (x ': xs)   = 1 + (MLength xs)
    MLength '[]         = 0

data (f :+: g) e
    = Inl (f e)
    | Inr (g e)

data (f :*: g) e
     = Prod (f e, g e)

data TAlg a
    = TPrim a
    | TSum (TAlg a) (TAlg a)
    | TProd (TAlg a) (TAlg a)

type family ADT (a :: TAlg (k -> Type)) where
    ADT (TPrim a)   = a
    ADT (TSum a b)  = ADT a :+: ADT b
    ADT (TProd a b) = ADT a :*: ADT b

type family Subsumes (a :: TAlg k) (b :: TAlg k) :: Bool where
    Subsumes (TPrim x) (TPrim x)    = True
    Subsumes (TPrim x) (TSum a b)   = Subsumes (TPrim x) a `TOr` Subsumes (TPrim x) b
    -- ... TODO
    Subsumes _         _            = False

type family TOr (a :: Bool) (b :: Bool) where
    TOr True _ = True
    TOr _ True = True
    TOr False False = False

-- Testing

data VConst e = VConst Int  deriving (Eq, Show, Functor)
data VPair e = VPair e e    deriving (Eq, Show, Functor)
data OMult e = OMult e e    deriving (Eq, Show, Functor)
data OFst e = OFst e        deriving (Eq, Show, Functor)
data OSnd e = OSnd e        deriving (Eq, Show, Functor)

type Value = ADT (TSum (TPrim VConst) (TPrim VPair))

{- How to GHCI this:
    stack exec ghci -- -isrc -XFlexibleContexts -XTypeFamilies -XPolyKinds -XDataKinds src/Sky/Compositional/TypeShiet.hs
    :kind! TOr False True
-}
