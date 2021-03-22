
{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, FunctionalDependencies #-}

-- | Just a playground for experiments here

module Sky.Graph.TestGraph where

import Data.Maybe (isJust)

class StupidGraph g v e | g -> v, g -> e where
    -- No equality
    -- No enumeration
    -- No (de)construction of edges
    travel :: g -> v -> e -> Maybe v
    isValidEdge :: g -> v -> e -> Bool
    --isValidEdge = (.).(.).(.) isJust travel
    isValidEdge g v e = isJust $ travel g v e

class StupidGraph g v e => LessStupidGraph g v e where
    -- No equality
    -- No enumeration
    -- Destination of edge requires edge to exist
    makeEdge :: g -> v -> v -> e
    sourceV :: g -> e -> v
    -- Laws
    --  sourceEV . makeEdge v1 v2 = v1 
    hasEdge :: g -> e -> Bool
    hasEdge g e = isValidEdge g (sourceV g e) e
    hasEdgeV :: g -> v -> v -> Bool
    hasEdgeV g v1 v2 = isValidEdge g v1 $ makeEdge g v1 v2

class LessStupidGraph g v e => SimpleGraph g v e  where
    -- No direct enumeration, could travel connected parts
    eqV :: g -> v -> v -> Bool
    destV :: g -> e -> v
    -- Laws
    --  forall v. eq v v
    tupleE :: g -> e -> (v, v)
    tupleE g e = (sourceV g e, destV g e)
    fromTupleE :: g -> (v, v) -> e
    fromTupleE g (v1,v2) = makeEdge g v1 v2
    eqE :: g -> e -> e -> Bool
    eqE g e1 e2 =  (eqV g (sourceV g e1) (sourceV g e2))
                && (eqV g (destV   g e1) (destV   g e2))

class SimpleGraph g v e => UsefulGraph g v e where
    vertices :: g -> [v]
    -- Base impls
    outV :: g -> v -> [v]
    outV g v = filter (hasEdgeV g v) (vertices g)
    inV :: g -> v -> [v]
    inV g v = filter (\vi -> hasEdgeV g vi v) (vertices g)
    outE :: g -> v -> [e]   -- => sourceV g e = v
    outE g v = fmap (makeEdge g v) (outV g v)
    inE :: g -> v -> [e]    -- => destV g e = v
    inE g v = fmap (\vi -> makeEdge g vi v) (inV g v)
    edges :: g -> [e]       -- => hasEdge g e
    edges g = concatMap (outE g) (vertices g)

-- Utilities
indegree :: forall a g v. UsefulGraph g v a => g -> v -> Int
indegree g v = length (inE g v) 
outdegree :: forall a g v. UsefulGraph g v a => g -> v -> Int
outdegree g v = length (outE g v)

---- Laws for undirected graphs
--hasEdge v1 v2 = hasEdge v2 v1

