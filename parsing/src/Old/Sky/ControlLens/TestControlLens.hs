
{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}

module Old.Sky.ControlLens.TestControlLens where

import Data.HashMap.Strict
import Control.Monad.Trans.Class
import Control.Monad.State.Class
import Control.Monad.State.Strict (StateT, runStateT, execStateT)
import Data.Functor.Identity

import Control.Lens

-- Usage: ghci -ddump-splices 

-- Some very simple tests for Control.Lens -------------------------------------

data Foo a = Foo { _bar :: Int, _baz :: Int, _quux :: a }
  deriving (Eq, Show)
makeLenses ''Foo
xFoo :: Foo Int
xFoo = Foo 1 2 3

data MyADT =
    ADTSingle Int
  | ADTTuple Bool Int
  deriving (Eq, Show)
makeLenses ''MyADT
xADT :: MyADT
xADT = ADTTuple True 5
-- Result: doesn't get us anywhere

-- Make custom lenses for those
adtInt :: Lens' MyADT Int
adtInt = lens get set where
  get :: MyADT -> Int
  get (ADTSingle i) = i
  get (ADTTuple _ i) = i
  set :: MyADT -> Int -> MyADT
  set (ADTSingle _) j = ADTSingle j
  set (ADTTuple v _) j = ADTTuple v j

-- This is wrong, it should be a "Setting" or something which my fail
adtBool :: Lens' MyADT Bool
adtBool = lens get set where
  get :: MyADT -> Bool
  get (ADTSingle _) = error "Total fail"
  get (ADTTuple v _) = v
  set :: MyADT -> Bool -> MyADT
  set (ADTSingle _) _ = error "Total fail"
  set (ADTTuple _ j) v = ADTTuple v j
