
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE RankNTypes             #-}
--{-# LANGUAGE MultiParamTypeClasses  #-}
--{-# LANGUAGE TypeFamilies           #-}

-- TODO: Scrap names, just use integers. Users can add names as "metadata".

module Sky.Util.GraphMonad 
    ( Typeable      -- needed whenever you use it, so re-export
    , Reference
    , GraphT
    , RefName
    , evalGraphT
    , makeReference
    , makeUnnamedReference
    , resolveReference
    --, updateReference
    , getReferenceName
    , letRec
    ) where

import Prelude hiding (lookup, (!!))
import Data.Maybe
import Data.Dynamic
import Control.Monad.State.Strict
import Sky.Util.NewContainer

data Reference k v = Reference k

newtype GraphState k = GraphState
    { graphMap :: HashMap k Dynamic
    }

type GraphT k m a = StateT (GraphState k) m a
--type RefGenerator k = forall v. HashMap k v -> Maybe k -> k

class (Show k, Monoid k, Ord k, Hashable k) => RefName k where
    unnamed :: k
    makeSuffix :: Int -> k

instance RefName String where
    unnamed = "unnamed"
    makeSuffix = show

--defaultRefGenerator :: RefGenerator String
--defaultRefGenerator map suggestion = let
--    tryName :: String -> String -> String
--    tryName name alt = if not $ hasKey map name then name else alt
--    genName :: String -> Int -> String
--    genName name i = tryName (name ++ show i) (genName name (i+1))
--    in case suggestion of
--        Nothing -> genName "unnamed" 0
--        Just n  -> tryName n (genName n 0)

--{- When a datastructure has references of type r, we want to have
--    some control over them:
--        Functor-like:   Apply a function over all references
--                        e.g. rename
---}

--mkRefName :: v -> k -> GraphT 

gmState :: forall k m a. (Monad m) => (HashMap k Dynamic -> (a, HashMap k Dynamic)) -> GraphT k m a
gmState mod = state stateMod where
    stateMod :: GraphState k -> (a, GraphState k)
    stateMod (GraphState g) = (result, GraphState g') where
        (result, g') = mod g

gmHasReference :: (Monad m, RefName k) => k -> GraphT k m Bool
gmHasReference name = gmState hasReference where
    hasReference map = (hasKey map name, map)

gmStore :: (Monad m, RefName k, Typeable v) => k -> v -> GraphT k m ()
gmStore k v = gmState put where
    put map = ((), insert (k, toDyn v) map)

gmLookup :: forall m k v. (Monad m, RefName k, Typeable v) => k -> GraphT k m v
gmLookup k = gmState $ dummy where
    dummy :: HashMap k Dynamic -> (v, HashMap k Dynamic)
    dummy map = (v, map) where
        reqType :: TypeRep
        reqType = typeRep (Proxy :: Proxy v)
        errKey :: Dynamic
        errKey      = error $ "Non existant key: " ++ show k
        errType :: v
        errType     = error $ "Wrong datatype for entry: "
                        ++ show k ++ " is " ++ show (dynTypeRep $ map !! k)
                        ++ " but requested was " ++ show reqType
        dynValue :: Dynamic
        dynValue = fromMaybe errKey (lookup k map)
        doCast :: Dynamic -> v
        doCast dyn = fromDyn dyn errType
        v :: v
        v = doCast dynValue

gmTryName :: (Monad m, RefName k) => k -> GraphT k m k -> GraphT k m k
gmTryName name alt = do
    exists <- gmHasReference name
    if not exists
        then return name
        else alt

gmIterateNames :: (Monad m, RefName k) => k -> Int -> GraphT k m k
gmIterateNames name i = gmTryName (name `mappend` makeSuffix i) $ gmIterateNames name (i+1)

gmGenerateName :: (Monad m, RefName k) => k -> GraphT k m k
gmGenerateName suggestedName = gmTryName suggestedName $ gmIterateNames suggestedName 0 where

gmGenerateUnnamed :: (Monad m, RefName k) => GraphT k m k
gmGenerateUnnamed = gmIterateNames unnamed 0 where

----gmMakeReference :: forall k v m. (Monad m, RefName k) => k -> v -> GraphT k v m k
----gmMakeReference suggestedName value = do
----        name <- gmGenerateName suggestedName
----        gmStore name value
----        return name

evalGraphT :: (Monad m, RefName k) => GraphT k m a -> m a
evalGraphT = flip evalStateT (GraphState empty)

letRec :: (Monad m, RefName k, Typeable v) => k -> (Reference k v -> GraphT k m v) -> GraphT k m (Reference k v)
letRec name' inner = do
        name <- gmGenerateName name'
        value <- inner (Reference name)
        gmStore name value
        return (Reference name)

resolveReference :: (Monad m, RefName k, Typeable v) => (Reference k v) -> GraphT k m v
resolveReference (Reference name) = gmLookup name

--updateReference :: (Monad m, RefName k, Typeable v) => (Reference k v) -> v -> GraphT k m ()
--updateReference (Reference name) value = gmStore name value

getReferenceName :: Reference k v -> k
getReferenceName (Reference name) = name

makeReference :: (Monad m, RefName k, Typeable v) => k -> v -> GraphT k m (Reference k v)
makeReference name' value = do
        name <- gmGenerateName name'
        gmStore name value
        return (Reference name)

makeUnnamedReference :: (Monad m, RefName k, Typeable v) => v -> GraphT k m (Reference k v)
makeUnnamedReference value = do
        name <- gmGenerateUnnamed
        gmStore name value
        return (Reference name)
