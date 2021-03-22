
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}

{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}

{- | Compositional data types
See http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.302.6303&rep=rep1&type=pdf
    http://bahr.io/pubs/files/bahr11wgp-slides%20(full).pdf
And https://hackage.haskell.org/package/compdata
-}

module Sky.Compositional.Algebra where

import Control.Monad ((<=<))
import Data.Functor.Const (Const)

----------------------------------------------------------------------------------------------------
-- Essentially Data.Fix

-- Initial f-Algebra
data Term f = Term (f (Term f))

unTerm :: Term f -> f (Term f)
unTerm (Term x) = x

instance Show (f (Term f)) => Show (Term f) where
    show (Term x) = "(" ++ show x ++ ")"
  
instance Eq (f (Term f)) => Eq (Term f) where
    (Term a) == (Term b) = a == b

-- | Algebra
type Alg f a = f a -> a

-- | Coalgebra
type CoAlg f a = a -> f a

----------------------------------------------------------------------------------------------------
-- Datatype composition (Essentially "Either")

infixr 6 :+:

data (f :+: g) e
    = Inl (f e)
    | Inr (g e)

instance (Show (f e), Show (g e)) => Show ((f :+: g) e) where
    show (Inl a) = show a
    show (Inr a) = show a

instance (Eq (f e), Eq (g e)) => Eq ((f :+: g) e) where
    (Inl a) == (Inl b) = a == b
    (Inr a) == (Inr b) = a == b
    _ == _ = False

instance (Functor f, Functor g) => Functor (f :+: g) where
    fmap f (Inl a) = Inl $ fmap f a
    fmap f (Inr a) = Inr $ fmap f a

instance (Foldable f, Foldable g) => Foldable (f :+: g) where
    foldMap :: Monoid m => (a -> m) -> (f :+: g) a -> m
    foldMap f (Inl a) = foldMap f a
    foldMap f (Inr a) = foldMap f a

instance (Traversable f, Traversable g) => Traversable (f :+: g) where
    sequenceA :: Applicative x => (f :+: g) (x a) -> x ((f :+: g) a)
    sequenceA (Inl a) = Inl <$> sequenceA a
    sequenceA (Inr a) = Inr <$> sequenceA a

----------------------------------------------------------------------------------------------------
-- Subsumption, injection and projection

infixl 5 :<:

class f :<: g where
    inj :: f a -> g a
    proj :: g a -> Maybe (f a)

instance f :<: f where
    inj = id
    proj = Just

instance {-# OVERLAPPABLE #-} f :<: (f :+: g) where
    inj = Inl . inj
    proj (Inl f) = Just f
    proj (Inr _) = Nothing

instance {-# OVERLAPPABLE #-} (f :<: g) => (f :<: (h :+: g)) where
    inj = Inr . inj
    proj (Inl _) = Nothing
    proj (Inr g) = proj g

inject :: (g :<: f) => g (Term f) -> Term f
inject = Term . inj

project :: (g :<: f) => Term f -> Maybe (g (Term f))
project (Term t) = proj t

----------------------------------------------------------------------------------------------------
-- Catamorphism, Anamorphism

cata :: Functor f => (f a -> a) -> (Term f -> a)
cata f = f . fmap (cata f) . unTerm

ana :: Functor f => (a -> f a) -> (a -> Term f)
ana f = Term . fmap (ana f) . f

cataM :: (Traversable f , Monad m) => (f a -> m a) -> (Term f -> m a)
cataM f = f <=< mapM (cataM f) . unTerm

anaM :: (Traversable f , Monad m) => (a -> m (f a)) -> (a -> m (Term f))
anaM f = (Term <$>) . mapM (anaM f) <=< f

----------------------------------------------------------------------------------------------------
-- Datatype product (Essentially tuples)

infixr 8 :*:

data (f :*: g) e = f e :*: g e
data FU_SUBLIME_A = FU_SUBLIME_A

instance (Show (f e), Show (g e)) => Show ((f :*: g) e) where
    show (a :*: b) = show a ++ " " ++ show b

instance (Eq (f e), Eq (g e)) => Eq ((f :*: g) e) where
    (a :*: b) == (a' :*: b')    = (a == a') && (b == b')

instance (Functor f, Functor g) => Functor (f :*: g) where
    fmap f (a :*: b) = fmap f a :*: fmap f b

instance (Foldable f, Foldable g) => Foldable (f :*: g) where
    foldMap :: Monoid m => (a -> m) -> (f :*: g) a -> m
    foldMap f (a :*: b) = foldMap f a `mappend` foldMap f b

instance (Traversable f, Traversable g) => Traversable (f :*: g) where
    sequenceA :: Applicative x => (f :*: g) (x a) -> x ((f :*: g) a)
    sequenceA (a :*: b) = (:*:) <$> sequenceA a <*> sequenceA b

getL :: (f :*: g) a -> f a
getL (a :*: b) = a

getR :: (f :*: g) a -> g a
getR (a :*: b) = b

distributeP :: ((f :+: g) :*: r) a -> ((f :*: r) :+: (g :*: r)) a
distributeP ((Inl a) :*: r) = Inl (a :*: r)
distributeP ((Inr b) :*: r) = Inr (b :*: r)

collectP :: ((f :*: r) :+: (g :*: r)) a -> ((f :+: g) :*: r) a
collectP (Inl (a :*: r)) = (Inl a) :*: r
collectP (Inr (b :*: r)) = (Inr b) :*: r

stripR :: (Functor f, Functor g) => Term (f :*: g) -> Term f
stripR = cata (Term . getL)

stripL :: (Functor f, Functor g) => Term (f :*: g) -> Term g
stripL = cata (Term . getR)

liftL :: (Functor f, Functor g) => (Term f -> a) -> (Term (f :*: g) -> a)
liftL alg = alg . stripR

liftR :: (Functor f, Functor g) => (Term g -> a) -> (Term (f :*: g) -> a)
liftR alg = alg . stripL

----------------------------------------------------------------------------------------------------

infixr 7 :&:

type (f :&: a) e = (f :*: Const a) e

----------------------------------------------------------------------------------------------------
-- Subsumption with products

infixl 5 :<*:

class f :<*: g where
    injProduct :: f a -> g a
    projProduct :: g a -> Maybe (f a)

instance (f :<*: g) => (f :*: h) :<*: (g :*: h) where
    injProduct :: (f :*: h) a -> (g :*: h) a
    injProduct = undefined
    projProduct :: (g :*: h) a -> Maybe ((f :*: h) a)
    projProduct = undefined

-- instance f :<: f where
--     inj = id
--     proj = Just

-- instance {-# OVERLAPPABLE #-} f :<: (f :+: g) where
--     inj = Inl . inj
--     proj (Inl f) = Just f
--     proj (Inr _) = Nothing

-- instance {-# OVERLAPPABLE #-} (f :<: g) => (f :<: (h :+: g)) where
--     inj = Inr . inj
--     proj (Inl _) = Nothing
--     proj (Inr g) = proj g

-- inject :: (g :<: f) => g (Term f) -> Term f
-- inject = Term . inj

-- project :: (g :<: f) => Term f -> Maybe (g (Term f))
-- project (Term t) = proj t
