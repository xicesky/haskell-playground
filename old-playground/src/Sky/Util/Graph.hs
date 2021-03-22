
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
-- {-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeFamilies           #-}

module Sky.Util.Graph where

import Prelude hiding (lookup, (!!))
import Data.Dynamic
import Sky.Util.NewContainer

----------------------------------------------------------------------------------------------------

class (Show k, Monoid k, Ord k, Hashable k) => RefName k where
    unnamed :: k
    makeSuffix :: Int -> k

instance RefName String where
    unnamed = "unnamed"
    makeSuffix = show

----------------------------------------------------------------------------------------------------

class GraphValue v where
    type RefT v :: *
    reference :: RefT v -> v

----------------------------------------------------------------------------------------------------

type Graph k v = HashMap k v
type DynGraph k v = HashMap k Dynamic

----------------------------------------------------------------------------------------------------

gTryName :: (RefName k) => Graph k v -> k -> k -> k
gTryName graph name alt = if graph `hasKey` name then alt else name

gIterateNames :: (RefName k) => Graph k v -> k -> Int -> k
gIterateNames graph name i = gTryName graph (name `mappend` makeSuffix i) $ gIterateNames graph name (i+1)

----------------------------------------------------------------------------------------------------
-- Demo

-- data Expr r
--     = Literal Int
--     | Add (Expr r) (Expr r)
--     | Mul (Expr r) (Expr r)
--     | Reference r
--     deriving (Show, Eq, Ord)

-- type ExprGraph r = Graph r (Expr r)

-- instance (Ord r, Hashable r) => GraphValue (Expr r) where
--     type RefT (Expr r) = r
--     reference :: r -> Expr r
--     reference = Reference

-- eval :: (Ord r, Hashable r) => ExprGraph r -> Expr r -> Int
-- eval m (Literal i) = i
-- eval m (Add x y) = eval m x + eval m y
-- eval m (Mul x y) = eval m x * eval m y
-- eval m (Reference r) = eval m (m !! r)

-- demoExpr :: ExprGraph String 
-- demoExpr = fromList
--     [ ("a", Literal 5)
--     , ("b", Add (Reference "a") (Literal 1))
--     , ("main", Mul (Reference "a") (Reference "b"))
--     ]

-- demo = eval demoExpr (Reference "main")
