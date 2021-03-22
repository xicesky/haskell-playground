
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

{-# LANGUAGE PolyKinds              #-}
--{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE ConstraintKinds        #-}

module Old.Sky.Compositional.Demo4Contexts where

import Control.Monad.Writer.Lazy (Writer, runWriter, tell)

import Old.Sky.Compositional.Algebra
import Old.Sky.Compositional.Demo1Basics
    (   Val(..), Op(..), Sug(..)
    ,   iConst, iPair, iFst, iSnd, iMult
    ,   VTerm, Exp, ETerm, Exp', ETerm'
    ,   eval
    )

----------------------------------------------------------------------------------------------------
-- Example annotation (e.g. line info from the parser...)

data Pos e
    = Pos String
    deriving (Eq, Show, Functor, Foldable, Traversable)
--  = Const String e

{- Reminder:
type Exp = Val :+: Op
type Exp' = Sug :+: Exp

type VTerm = Term Val
type ETerm = Term Exp
type ETerm' = Term Exp'
-}

type EPos = Exp :*: Pos
type EPosTerm = Term EPos

type EPos' = Exp' :*: Pos
type EPosTerm' = Term EPos'

----------------------------------------------------------------------------------------------------

{- So we have
    eval :: (Functor f, Eval f g) => Term f -> Term g
which specialises to
    eval' :: ETerm -> VTerm
and can lift it to terms with position info:
-}

eval' :: ETerm -> VTerm
eval' = eval

evalPos :: EPosTerm -> VTerm
evalPos = liftL eval'

----------------------------------------------------------------------------------------------------

{- But what if we have a term algebra that uses the position info and still want to compose it with
    another term algebra? For example our desugaring could use the position info to log the location
    where desugaring took place...

    Instead of:
        instance (...) Desug Sug f where
            desugAlg :: Sug (Term f) -> Term f
    Which then lifts to the constraint:
        Desug (Sug :+: Exp :+: Val) f

    It would have a signature quite like:
        instance (...) Desug Sug f where
            (Sug :*: Pos) (Term f) -> LogM (Term f)
    Which would lift to the constraint:
        Desug ((Sug :*: Pos) :+: Exp :+: Val) f
    Instead of the desired
        Desug ((Sug :+: Exp :+: Val) :*: Pos) f

    We can try this:
-}

type LogM a = Writer [String] a

class DesugReport f g where
    desugReportAlg :: f (Term g) -> LogM (Term g)

instance (DesugReport (f :*: h) v, DesugReport (g :*: h) v) => DesugReport ((f :+: g) :*: h) v where
    desugReportAlg (Inl a :*: p) = desugReportAlg (a :*: p)
    desugReportAlg (Inr a :*: p) = desugReportAlg (a :*: p)


-- instance (DesugReport f v, DesugReport g v) => DesugReport (f :+: g) v where
--     desugReportAlg :: (f :+: g) (Term v) -> LogM (Term v)
--     desugReportAlg (Inl a) = desugReportAlg a
--     desugReportAlg (Inr a) = desugReportAlg a

-- instance {-# OVERLAPPABLE #-} (g :<: f) => DesugReport g f where
--     desugReportAlg :: g (Term f) -> LogM (Term f)
--     desugReportAlg = return . inject

-- iConst2 :: (Val :<: f) => Int -> Term (f :*: Pos)
-- iConst2 x = inject (Const x)
-- iPair2 :: (Val :<: f) => Term (f :*: Pos) -> Term (f :*: Pos) -> Term (f :*: Pos)
-- iPair2 x y = inject (Pair x y)


-- instance (Val :<: f, Op :<: f) => DesugReport (Sug :*: Pos) (f :*: Pos) where
--     desugReportAlg :: (Sug :*: Pos) (Term (f :*: Pos)) -> LogM (Term (f :*: Pos))
--     desugReportAlg (Neg x :*: p) = return $ iConst (-1) `iMult` x :*: p
--     desugReportAlg (Swap x :*: p) = return $ iPair (iSnd x) (iFst x) :*: p

--desug :: ETerm' -> ETerm
-- desugReport :: ETerm' -> LogM ETerm
-- desugReport = cataM desugReportAlg

-- desugReport :: EPosTerm' -> LogM EPosTerm
-- desugReport = cataM desugReportAlg

{- You can think of the type class inference progressing like this:
        DesugReport EPos' EPos
    ==  DesugReport ((Sug :+: Val :+: Op) :*: Pos) EPos
    <=  DesugReport (Sug :*: Pos) EPos , DesugReport ((Val :+: Op) :*: Pos) EPos
    <=  DesugReport (Sug :*: Pos) EPos , DesugReport (Val :*: Pos) EPos , DesugReport (Op :*: Pos) EPos
-}

data TestProxy c = TestProxy

instance c => Show (TestProxy c) where
    show (TestProxy) = "Ok"

-- testConstraint :: (f :<: g) => TestProxy (f :<: g) -> String
-- testConstraint _ = "Ok"
