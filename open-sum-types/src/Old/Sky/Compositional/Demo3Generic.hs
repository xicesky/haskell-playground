
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}

{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}

module Old.Sky.Compositional.Demo3Generic where

----------------------------------------------------------------------------------------------------

data Term f l = Term (f (Term f) l)

type f :-> g = forall i . f i -> g i

class HFunctor (h :: (* -> *) -> * -> *) where
    hfmap :: forall a b j. (forall i. a i -> b i) -> h a j -> h b j

----------------------------------------------------------------------------------------------------

infixr 6 :+:

data (f :+: g) (e :: * -> *) l
    = Inl (f e l)
    | Inr (g e l)

instance (HFunctor f, HFunctor g) => HFunctor (f :+: g) where
    --hfmap :: (a :-> b) -> (f :+: g) a :-> (f :+: g) b
    hfmap f (Inl a) = Inl $ hfmap f a
    hfmap f (Inr a) = Inr $ hfmap f a

----------------------------------------------------------------------------------------------------

-- |The subsumption relation.
infixl 5 :<:

--type HMaybe f g = forall i. f i -> Maybe (g i)
--type NatM m f g = forall i. f i -> m (g i)

class (sub :: (* -> *) -> * -> *) :<: sup where
    inj :: sub a i -> sup a i
    --proj :: NatM Maybe (sup a) (sub a)
    proj :: (sup a) i -> Maybe ((sub a) i) 

instance (:<:) f f where
    inj = id
    proj = Just

instance (:<:) f (f :+: g) where
    inj = Inl
    proj (Inl x) = Just x
    proj (Inr _) = Nothing

instance (f :<: g) => (:<:) f (h :+: g) where
    inj = Inr . inj
    proj (Inr x) = proj x
    proj (Inl _) = Nothing

inject :: (g :<: f) => g (Term f) l -> Term f l
inject = Term . inj

project :: (g :<: f) => Term f l -> Maybe (g (Term f) l)
project (Term t) = proj t       -- t :: f (Term f) l

----------------------------------------------------------------------------------------------------

--type Alg f e = f e :-> e

cata :: forall f a. HFunctor f => (forall i. f a i -> a i) -> (forall j. Term f j -> a j)
cata f = run 
    where run :: Term f i -> a i
          run (Term t) = f (hfmap run t)

----------------------------------------------------------------------------------------------------

data Val e l where
    Const       ::              Int     ->  Val e Int
    Pair        ::  e s     ->  e t     ->  Val e (s, t)

instance HFunctor Val where
    hfmap _ (Const i)   = Const i
    hfmap f (Pair x y)  = Pair (f x) (f y)

iConst :: (Val :<: f) => Int -> Term f Int
iConst x = inject (Const x)     -- Const x  :: Val e Int
iPair :: (Val :<: f) => Term f s -> Term f t -> Term f (s, t)
iPair x y = inject (Pair x y)

-- Unsafe (!!)
projConst :: (Val :<: f) => Term f Int -> Int
projConst v = case project v of Just (Const n) -> n
projPair :: (Val :<: f) => Term f (s, t) -> (Term f s, Term f t)
projPair v = case project v of Just (Pair x y) -> (x, y)

----------------------------------------------------------------------------------------------------

data Op e l where
    Mult        ::  e Int   ->  e Int   ->  Op e Int
    Fst         ::             e (s, t) ->  Op e s
    Snd         ::             e (s, t) ->  Op e t

instance HFunctor Op where
    hfmap f (Mult x y)  = Mult (f x) (f y)
    hfmap f (Fst x)     = Fst (f x)
    hfmap f (Snd x)     = Snd (f x)

iMult :: (Op :<: f) => Term f Int -> Term f Int -> Term f Int
iMult x y = inject (Mult x y)
iFst :: (Op :<: f) => Term f (s, t) -> Term f s
iFst x = inject (Fst x)
iSnd :: (Op :<: f) => Term f (s, t) -> Term f t
iSnd x = inject (Snd x)

----------------------------------------------------------------------------------------------------
-- "Eval" Algebra
-- Removes "Op" from our AST, substituting it to val

class Eval f v where
    evalAlg :: f (Term v) i -> Term v i

instance (Eval f v, Eval g v) => Eval (f :+: g) v where
    evalAlg :: (f :+: g) (Term v) i -> (Term v) i
    evalAlg (Inl a) = evalAlg a
    evalAlg (Inr a) = evalAlg a

instance {-# OVERLAPPABLE #-} (g :<: f) => Eval g f where
    evalAlg :: g (Term f) i -> Term f i
    evalAlg = inject

instance (Val :<: v) => Eval Op v where
    evalAlg :: Op (Term v) i -> Term v i
    evalAlg (Mult x y) = iConst $ projConst x * projConst y
    evalAlg (Fst p) = fst $ projPair p
    evalAlg (Snd p) = snd $ projPair p

eval :: (HFunctor f, Eval f g) => Term f i -> Term g i
eval = cata evalAlg

----------------------------------------------------------------------------------------------------
-- "EvalI" Algebra
-- Evaluates values to their Haskell counterpart

data I a = I { unI :: a }       -- Identity
data K a b = K { unK :: a }     -- Constant

--type Alg f e = f e :-> e

class EvalI f where
    --evalAlgI :: Alg f I
    evalAlgI :: f I i -> I i

instance (EvalI f, EvalI g) => EvalI (f :+: g) where
    evalAlgI :: (f :+: g) I i -> I i
    evalAlgI (Inl a) = evalAlgI a
    evalAlgI (Inr a) = evalAlgI a

instance EvalI Val where
    evalAlgI :: Val I i -> I i
    evalAlgI (Const x) = I $ x
    evalAlgI (Pair x y) = I $ (unI x, unI y)

instance EvalI Op where
    evalAlgI :: Op I i -> I i
    evalAlgI (Mult x y) = I $ unI x * unI y
    evalAlgI (Fst x) = I $ fst (unI x)
    evalAlgI (Snd x) = I $ snd (unI x)

evalI :: (EvalI f, HFunctor f) => Term f t -> t
evalI = unI . cata evalAlgI
