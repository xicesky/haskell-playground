
{-# OPTIONS_GHC
    -Wno-unused-imports
    -Wno-unused-top-binds
    -Wno-unused-local-binds
    -Wno-unused-matches
    -Wno-name-shadowing
    -Wno-incomplete-patterns
#-}

{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}

{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}

module Old.Sky.Compositional.Demo1Basics where

import Old.Sky.Compositional.Algebra

-- For monadic demo only
import Control.Monad.Except (Except, runExcept, throwError, catchError)

----------------------------------------------------------------------------------------------------

data QQ = QQ

instance Show QQ where
    show QQ = "?"

class ShowC f where
    -- | Show constructor without contents
    showC :: f a -> String

instance (ShowC f, ShowC g) => ShowC (f :+: g) where
    showC (Inl a) = showC a
    showC (Inr a) = showC a

instance (Show (f QQ), Functor f) => ShowC f where
    showC = show . fmap (const QQ)

----------------------------------------------------------------------------------------------------

data Val e
    = Const Int
    | Pair e e
    deriving (Eq, Show, Functor)

-- instance ShowC Val where
--     showC (Const i) = show (Const i :: Val ())
--     showC (Pair _ _) = "Pair"

instance Foldable Val where
    foldMap :: Monoid m => (a -> m) -> Val a -> m
    foldMap f (Const _) = mempty
    foldMap f (Pair x y) = (mempty `mappend` f x) `mappend` f y

instance Traversable Val where
    sequenceA :: Applicative f => Val (f a) -> f (Val a)
    sequenceA (Const e) = pure (Const e)
    sequenceA (Pair x y) = Pair <$> x <*> y

iConst :: (Val :<: f) => Int -> Term f
iConst x = inject (Const x)
iPair :: (Val :<: f) => Term f -> Term f -> Term f
iPair x y = inject (Pair x y)

-- Unsafe (!!)
projConst :: (Val :<: f) => Term f -> Int
projConst v = case project v of Just (Const n) -> n
projPair :: (Val :<: f) => Term f -> (Term f, Term f)
projPair v = case project v of Just (Pair x y) -> (x, y)

----------------------------------------------------------------------------------------------------

data Op e
    = Mult e e
    | Fst e
    | Snd e
    deriving (Eq, Show, Functor)

-- instance ShowC Op where
--     showC (Mult _ _) = "Mult"
--     showC (Fst _) = "Fst"
--     showC (Snd _) = "Snd"

instance Foldable Op where
    foldMap :: Monoid m => (a -> m) -> Op a -> m
    foldMap f (Mult x y) = (mempty `mappend` f x) `mappend` f y
    foldMap f (Fst x) = mempty `mappend` f x
    foldMap f (Snd x) = mempty `mappend` f x

instance Traversable Op where
    sequenceA :: Applicative f => Op (f a) -> f (Op a)
    sequenceA (Mult x y) = Mult <$> x <*> y
    sequenceA (Fst x) = Fst <$> x
    sequenceA (Snd x) = Snd <$> x

iFst :: (Op :<: f) => Term f -> Term f
iFst x = inject (Fst x)
iSnd :: (Op :<: f) => Term f -> Term f
iSnd x = inject (Snd x)
iMult :: (Op :<: f) => Term f -> Term f -> Term f
iMult x y = inject (Mult x y)

----------------------------------------------------------------------------------------------------

data Sug e
    = Neg e
    | Swap e
    deriving (Eq, Show, Functor, Foldable, Traversable)

-- instance ShowC Sug where
--     showC (Neg _) = "Neg"
--     showC (Swap _) = "Swap"

iNeg :: (Sug :<: f) => Term f -> Term f
iNeg = inject . Neg
iSwap :: (Sug :<: f) => Term f -> Term f
iSwap = inject . Swap

type Exp = Val :+: Op
type Exp' = Sug :+: Exp

type VTerm = Term Val
type ETerm = Term Exp
type ETerm' = Term Exp'

----------------------------------------------------------------------------------------------------
-- Algebra

class Eval f v where
    evalAlg :: f (Term v) -> (Term v)

instance (Eval f v, Eval g v) => Eval (f :+: g) v where
    evalAlg :: (f :+: g) (Term v) -> (Term v)
    evalAlg (Inl a) = evalAlg a
    evalAlg (Inr a) = evalAlg a

instance {-# OVERLAPPABLE #-} (g :<: f) => Eval g f where
    evalAlg :: g (Term f) -> Term f
    evalAlg = inject    -- inject :: (g :<: f) => g (Term f) -> Term f

instance (Val :<: v) => Eval Op v where
    evalAlg :: Op (Term v) -> Term v
    evalAlg (Mult x y) = iConst $ projConst x * projConst y
    evalAlg (Fst p) = fst $ projPair p
    evalAlg (Snd p) = snd $ projPair p

--eval :: ETerm -> VTerm
eval :: (Functor f, Eval f g) => Term f -> Term g
eval = cata evalAlg

class Desug f g where
    desugAlg :: f (Term g) -> Term g

instance (Desug f v, Desug g v) => Desug (f :+: g) v where
    desugAlg :: (f :+: g) (Term v) -> Term v
    desugAlg (Inl a) = desugAlg a
    desugAlg (Inr a) = desugAlg a

instance {-# OVERLAPPABLE #-} (g :<: f) => Desug g f where
    desugAlg :: g (Term f) -> Term f
    desugAlg = inject

instance (Val :<: f, Op :<: f) => Desug Sug f where
    desugAlg :: Sug (Term f) -> Term f
    desugAlg (Neg x) = iConst (-1) `iMult` x
    desugAlg (Swap x) = iPair (iSnd x) (iFst x)

--desug :: ETerm' -> ETerm
desug :: (Functor f, Desug f g) => Term f -> Term g
desug = cata desugAlg

----------------------------------------------------------------------------------------------------
-- Example

expExample :: ETerm
expExample = iFst (iPair (iConst 2) (iConst 3)) `iMult` iConst 5

sugExample :: ETerm'
sugExample = iFst (iSwap (iPair (iConst 3) (iConst 2))) `iMult` iConst 5

failExample :: ETerm
failExample = iFst (iConst 3)

main :: IO ()
main = print $ (eval (desug sugExample :: ETerm) :: VTerm)

----------------------------------------------------------------------------------------------------
-- Monadic

newtype Error = Error String
    deriving (Show)

type ErrM a = Except Error a

-- orError :: Maybe x -> String -> ErrM x
-- orError (Just x)  msg = return x
-- orError (Nothing) msg = throwError msg

-- projConst :: (Val :<: f) => Term f -> Int
-- projConst v = case project v of Just (Const n) -> n
-- projPair :: (Val :<: f) => Term f -> (Term f, Term f)
-- projPair v = case project v of Just (Pair x y) -> (x, y)

pConst :: (Val :<: f) => Term f -> ErrM Int
pConst v = case project v of
    Just (Const n)  -> return n
    Just (v@_    )  -> throwError . Error $ "Expected an int but got value: " ++ showC v
    _               -> throwError . Error $ "Expected a value but got something else..."
pPair :: (Val :<: f) => Term f -> ErrM (Term f, Term f)
pPair v = case project v of
    Just (Pair x y) -> return (x, y)
    Just (v@_     ) -> throwError . Error $ "Expected a pair but got value: " ++ showC v
    _               -> throwError . Error $ "Expected a value but got something else..."

--evalFail :: ETerm -> VTerm

eval1 :: ETerm -> ErrM VTerm
-- Val
eval1 (Term (Inl (Const n)))    = return $ iConst n
eval1 (Term (Inl (Pair x y)))   = iPair <$> eval1 x <*> eval1 y
-- Op
eval1 (Term (Inr (Mult x y)))   = do
    x' <- eval1 x
    y' <- eval1 y
    case (unTerm x', unTerm y') of
        (Const m, Const n)  -> return $ iConst (m * n)
eval1 (Term (Inr (Fst p)))      = do
    p' <- eval1 p
    case unTerm p' of
        (Pair x y)          -> return $ x

class EvalFail f v where
    evalFailAlg :: f (Term v) -> ErrM (Term v)

instance (EvalFail f v, EvalFail g v) => EvalFail (f :+: g) v where
    evalFailAlg :: (f :+: g) (Term v) -> ErrM (Term v)
    evalFailAlg (Inl a) = evalFailAlg a
    evalFailAlg (Inr a) = evalFailAlg a

instance {-# OVERLAPPABLE #-} (g :<: f) => EvalFail g f where
    evalFailAlg :: g (Term f) -> ErrM (Term f)
    evalFailAlg = return . inject

instance (Val :<: v) => EvalFail Op v where
    evalFailAlg :: Op (Term v) -> ErrM (Term v)
    evalFailAlg (Mult x y) = fmap iConst $ (*) <$> pConst x <*> pConst y
    evalFailAlg (Fst p) = fst <$> pPair p
    evalFailAlg (Snd p) = snd <$> pPair p

--evalFail :: (Traversable f, EvalFail f g) => Term f -> ErrM (Term g)
evalFail :: ETerm -> ErrM VTerm
evalFail = cataM evalFailAlg
