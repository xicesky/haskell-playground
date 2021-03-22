
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Sky.Learn.GHCGenerics where

import GHC.Generics
import Data.Typeable

import Sky.Classes.Isomorphism.Monomorphic
import Sky.Implementations.Isomorphism
import Sky.Implementations.Isomorphism.MonoIso

data Stupid a = Stupid a
    deriving (Show, Eq, Generic)

data UserTree a
    = Leaf
    | Node a (UserTree a) (UserTree a)
    deriving (Show, Eq, Generic)

data Expr a b
    = LiteralA a
    | LiteralB b
    | LiteralAB a b
    | LiteralABA a b a
    | LiteralABAB a b a b
    deriving (Show, Eq, Generic)

{-  -- This is what Generics will create:
    M1 D _ (
            (
                M1 C _ (M1 S _ (K1 R Bool))
            :+: M1 C _ (M1 S _ (K1 R Bool))
        ) :+: (
                M1 C _ (M1 S _ (K1 R Bool) :*: M1 S _ (K1 R Bool))
            :+: (
                    M1 C _ (M1 S _ (K1 R Bool) :*: (M1 S _ (K1 R Bool) :*: M1 S _ (K1 R Bool)))
                :+: M1 C _ ((M1 S _ (K1 R Bool) :*: M1 S _ (K1 R Bool)) :*: (M1 S _ (K1 R Bool) :*: M1 S _ (K1 R Bool)))
            )
        )
    ) Bool
-}

{-  -- And this is Template Haskell:
    TyConI (DataD [] Sky.Learn.GHCGenerics.Expr [KindedTV a_1627402939 StarT,KindedTV b_1627402940 StarT]
        [NormalC Sky.Learn.GHCGenerics.LiteralA [(NotStrict,VarT a_1627402939)]
        ,NormalC Sky.Learn.GHCGenerics.LiteralB [(NotStrict,VarT b_1627402940)]
        ,NormalC Sky.Learn.GHCGenerics.LiteralAB [(NotStrict,VarT a_1627402939),(NotStrict,VarT b_1627402940)]
        ,NormalC Sky.Learn.GHCGenerics.LiteralABA [(NotStrict,VarT a_1627402939),(NotStrict,VarT b_1627402940),(NotStrict,VarT a_1627402939)]
        ,NormalC Sky.Learn.GHCGenerics.LiteralABAB [(NotStrict,VarT a_1627402939),(NotStrict,VarT b_1627402940),(NotStrict,VarT a_1627402939),(NotStrict,VarT b_1627402940)]
        ]
        []
        )
-}


stupidExample = Stupid False

example = Node 5 Leaf Leaf

example2 = LiteralABAB True False True False


{- Documentation reminder:
    M1          meta-info
    U1          unit (e.g. Leaf)

    K1 P        parameter
    K1 R        recursive

    L1          left argument of :+:    ("Left" on Either)
    R1          right argument of :+:   ("Right" on Either)
-}

class GPrint f where
    gprint :: f a -> String
    -- =

instance GPrint U1 where
    gprint _ = "()"

instance (GPrint a, GPrint b) => GPrint (a :*: b) where
    gprint :: (a :*: b) p -> String
    gprint _ = "(" ++ gprint (undefined :: a p) ++ ", " ++ gprint (undefined :: b p) ++ ")"

instance (GPrint a, GPrint b) => GPrint (a :+: b) where
    gprint :: (a :+: b) p -> String
    gprint _ = "(Either " ++ gprint (undefined :: a p) ++ " " ++ gprint (undefined :: b p) ++ ")"

instance (GPrint a) => GPrint (M1 i c a) where
    gprint :: M1 i c a p -> String
    gprint _ = gprint (undefined :: a p)

instance Typeable a => GPrint (K1 i a) where
    gprint :: K1 i a p -> String
    gprint _ = show $ typeRep (Proxy :: Proxy a)

printIsoType :: (Generic a, GPrint (Rep a)) => a -> String
printIsoType a = gprint (from a)

----------------------------------------------------------------------------------------------------
-- Isomorphism composition for algebraic data structures

class GIso t x where
    gIso :: Iso (t a) x

instance GIso U1 () where
    gIso = iso (\U1 -> ()) (\() -> U1)

instance (GIso a x, GIso b y) => GIso (a :*: b) (x, y) where
    gIso = iso from to where
        from (a :*: b) = (apply gIso a, apply gIso b)
        to (x, y) = (unapply gIso x :*: unapply gIso y)

instance (GIso a x, GIso b y) => GIso (a :+: b) (Either x y) where
    gIso = iso from to where
        from (L1 a) = Left $ apply gIso a
        from (R1 b) = Right $ apply gIso b
        to (Left x) = L1 $ unapply gIso x
        to (Right y) = R1 $ unapply gIso y

instance (GIso a x) => GIso (M1 i c a) x where
    gIso = iso from to where
        from (M1 a) = apply gIso a
        to x = M1 $ unapply gIso x

instance GIso (K1 i a) a where
    gIso = iso from to where
        from (K1 a) = a
        to x = K1 x

ii :: (Generic a, GIso (Rep a) b) => Iso a b
ii = iso aToRep repToA where
    aToRep a = apply gIso (from a)
    repToA r = to (unapply gIso r)

----------------------------------------------------------------------------------------------------
-- "Goedel numbers" for ADTs

class GNum t where
    gMax :: t a -> Int
    gNum :: t a -> Int -> Int
    gGoedel :: t a -> Int -> [Int]

instance GNum U1 where
    gMax :: U1 p -> Int
    gMax _ = 1
    gNum :: U1 p -> Int -> Int
    gNum U1 i = i
    gGoedel :: U1 p -> Int -> [Int]
    gGoedel _ i = [i]

instance Typeable a => GNum (K1 i a) where
    gMax :: K1 i a p -> Int
    gMax _ = 1
    gNum :: K1 i a p -> Int -> Int
    gNum (K1 a) i = i
    gGoedel :: K1 i a p -> Int -> [Int]
    gGoedel _ i = []

instance (GNum a, GNum b) => GNum (a :*: b) where
    gMax :: (a :*: b) p -> Int
    gMax _ = 1
    gNum :: (a :*: b) p -> Int -> Int
    gNum _ i = i
    gGoedel :: (a :*: b) p -> Int -> [Int]
    gGoedel _ i = []

instance (GNum a, GNum b) => GNum (a :+: b) where
    gMax :: (a :+: b) p -> Int
    gMax _ = gMax (undefined :: a p) + gMax (undefined :: b p)
    gNum :: (a :+: b) p -> Int -> Int
    gNum (L1 a) i = gNum a i
    gNum (R1 b) i = let
        left = gMax (undefined :: a p)
        in gNum b (i + left)
    gGoedel :: (a :+: b) p -> Int -> [Int]
    gGoedel _ i = []

instance (GNum a) => GNum (M1 i c a) where
    gMax :: M1 i c a p -> Int
    gMax _ = gMax (undefined :: a p)
    gNum :: M1 i c a p -> Int -> Int
    gNum (M1 x) i = gNum x i
    gGoedel :: M1 i c a p -> Int -> [Int]
    gGoedel _ i = []

cMax :: forall a. (Generic a, GNum (Rep a)) => a -> Int
cMax a = gMax (from a)

cNum :: forall a. (Generic a, GNum (Rep a)) => a -> Int
cNum a = gNum (from a) 0

----------------------------------------------------------------------------------------------------

---- Actual type: Iso a x1 -> Iso b x2 -> Iso (Either a b) (x1|x2)
--compose :: (x -> Bool) -> Iso a x -> Iso b x -> Iso (Either a b) x
--compose decision iso1 iso2 = iso 
