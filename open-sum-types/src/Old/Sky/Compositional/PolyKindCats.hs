
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}

{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE DataKinds              #-}

{-# LANGUAGE UndecidableInstances   #-}
-- {-# LANGUAGE TypeOperators          #-}

{- How to GHCI this:
    stack exec ghci -- -isrc -XFlexibleContexts -XTypeFamilies -XPolyKinds -XDataKinds src/Sky/Compositional/TypeShiet.hs
    :kind! TOr False True
-}

module Old.Sky.Compositional.PolyKindCats where

class Category hom where
    ident :: hom a a
    compose :: hom a b -> hom b c -> hom a c

instance Category (->) where
    ident = id
    compose = flip (.)

-- Natural transformations
newtype NatTrans f g = NatTrans { unNatTrans :: (forall a. f a -> g a) }

instance Category NatTrans where
    ident = NatTrans id
    compose f g = NatTrans (unNatTrans g . unNatTrans f)

-- Arbitrary functors
class HFunctor hom f where
    hmap :: hom a b -> hom (f a) (f b)

-- Check with Fixpoint and some data structure
data ListR r a
    = Nil
    | Cons a (r a)
    deriving (Eq, Show)

instance HFunctor (->) r => HFunctor (->) (ListR r) where
    hmap :: (a -> b) -> (ListR r a -> ListR r b)
    hmap f (Nil)  = Nil
    hmap f (Cons a r) = Cons (f a) (hmap f r)

instance HFunctor (NatTrans) (ListR) where
    hmap :: forall a b. (NatTrans a b) -> (NatTrans (ListR a) (ListR b))
    --hmap :: (forall x. a x -> b x) -> (forall y. List a y -> List b y)
    hmap (NatTrans f) = NatTrans g where
        g :: forall y. ListR a y -> ListR b y
        g (Nil)         = Nil
        g (Cons y ar)   = Cons y (f ar)

--data Fix (f :: k -> k)  = Fix (f (Fix f))     -- Does not work: f (Fix f) :: k is not a type (*)
--data Fix (f :: * -> *) = Fix (f (Fix f))

data Fix1 f     = Fix1 (f (Fix1 f))
data Fix2 f a   = Fix2 (f (Fix2 f) a)

instance Show (x (Fix2 x) a) => Show (Fix2 x a) where
    show (Fix2 x) = "(" ++ show x ++ ")"

instance HFunctor (->) (x (Fix2 x)) => HFunctor (->) (Fix2 x) where
    hmap :: (a -> b) -> (Fix2 x a -> Fix2 x b)
    hmap f (Fix2 xFix2a) = Fix2 $ hmap f xFix2a

type List = Fix2 ListR

-- instance HFunctor (->) (List) where
--     hmap :: (a -> b) -> (List a -> List b)
--     hmap f (Fix2 (Nil))         = Fix2 $ Nil
--     hmap f (Fix2 (Cons a xs))   = Fix2 $ Cons (f a) (hmap f xs)

intList :: List Int
intList = Fix2 $ Cons (-1) $ Fix2 $ Cons 1 $ Fix2 Nil

demo :: List Bool
demo = hmap (> 0) intList
