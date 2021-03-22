
{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}

module Sky.ControlLens.TestLensCompat where

import Prelude hiding (mod)
import Data.HashMap.Strict
import Control.Monad.Trans.Class
import Control.Monad.State.Class
import Control.Monad.State.Strict (StateT, runStateT)
import Data.Functor.Identity

import Control.Lens
import Sky.ControlLens.LensCompat

import qualified Sky.ControlLens.ExternalDefs as E

-- Basic code generator monad --------------------------------------------------

type SymbolTable = HashMap String Int

data CodegenState = CodegenState {
    _currentModule :: E.Module
  , _locals :: SymbolTable
  }
  deriving (Eq, Show)

newtype CodegenT m a = CodegenT { unCodegenT :: StateT CodegenState m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadState CodegenState )

defaultModule :: E.Module
defaultModule = E.Module { E.moduleName = "default", E.definitions = [] }
defaultState :: CodegenState
defaultState = CodegenState defaultModule empty

runCodegenT :: (Monad m) => CodegenT m a -> CodegenState -> m (a, E.Module)
runCodegenT cg initial = do 
  (val, st) <- runStateT (unCodegenT cg) initial
  return (val, _currentModule st)

newCodegenT :: (Monad m) => CodegenT m a -> m (a, E.Module)
newCodegenT codegen = runCodegenT codegen defaultState

-- Without lens

define2 :: (Monad m) => E.Type -> String -> CodegenT m ()
define2 t name = do
  modify $ \s -> let
    oldMod = _currentModule s
    oldDefs = E.definitions oldMod
    newDefs = oldDefs ++ [E.Definition name t]
    in s { _currentModule = oldMod { E.definitions = newDefs }}

-- Lenses for CodegenState -----------------------------------------------------

--makeLenses ''Definition
--makeLenses ''Module

makeDirectLenses ''E.Module
makeLenses ''CodegenState
makeDirectLenses ''E.Stuff

-- And here is why lenses are great:

define :: (Monad m) => E.Type -> String -> CodegenT m ()
define t name = currentModule . definitions %= (++ [E.Definition name t])

-- Test ------------------------------------------------------------------------

testCodegen :: E.Module
testCodegen = runIdentity $ do
  (dummy, mod) <- newCodegenT $ do
    define E.TVoid "putchar"
    define E.TInt "add"
    return ()
  return mod

main :: IO ()
main = do
  putStrLn $ show testCodegen

