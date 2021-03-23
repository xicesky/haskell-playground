
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Source: https://wiki.haskell.org/Sudoku#Monadic_Non-Deterministic_Solver
module NonDetSearch.HaskellWiki where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans

import Control.Monad.Identity
import NonDet.Class

newtype NondetT m a
  = NondetT { foldNondetT :: forall b. (a -> m b -> m b) -> m b -> m b }

runNondetT :: Monad m => NondetT m a -> m a
runNondetT m = foldNondetT m (\x _ -> return x) (error "No solution found.")

instance Functor m => Functor (NondetT m) where
  fmap f (NondetT g) = NondetT (\cons nil -> g (cons . f) nil)

instance Monad m => Applicative (NondetT m) where
    pure = return
    (<*>) = ap

instance Monad m => Monad (NondetT m) where
  return a = NondetT (\cons nil -> cons a nil)
  m >>= k  = NondetT (\cons nil -> foldNondetT m (\x -> foldNondetT (k x) cons) nil)

instance Monad m => Alternative (NondetT m) where
    empty = mzero
    (<|>) = mplus

instance Monad m => MonadPlus (NondetT m) where
  mzero         = NondetT (\_ nil -> nil)
  m1 `mplus` m2 = NondetT (\cons -> foldNondetT m1 cons . foldNondetT m2 cons)

instance MonadTrans NondetT where
  lift m = NondetT (\cons nil -> m >>= \a -> cons a nil)

newtype Nondet a = Nondet (NondetT Identity a)
    deriving (Functor, Applicative, Monad, Alternative, MonadPlus)

runNondet :: Nondet a -> a
runNondet (Nondet x) = runIdentity (runNondetT x)

foldNondet :: Nondet a -> (a -> b -> b) -> b -> b
foldNondet (Nondet nd) cons nil =
   runIdentity $ foldNondetT nd (\x xs -> return (cons x (runIdentity xs))) (return nil)

option :: (MonadPlus m) => [a] -> m a
option = msum . map return

{-----------------------------------------------------------------------------}
-- NonDet interface

instance NonDet Nondet

searchHWiki :: SFun
searchHWiki = SFun search where
    search :: Nondet a -> [a]
    search m = foldNondet m (:) []
