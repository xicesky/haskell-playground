
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving     #-}

module Old.Sky.Parsing.Invertible3.PartialType 
    ( PartialType
    , PartialType'
    , basicType
    , fullAlgebraicType
    , partialAlgebraicType
    , isOfType          -- shortcut "contains" for types
    --, contains          -- re-export from NewContainer
    , union             -- re-export from NewContainer
    , intersection      -- re-export from NewContainer
    , disjunct          -- re-export from NewContainer
    ) where

import Old.Sky.Util.NewContainer
import Old.Sky.Util.AllSet
import qualified Data.Set

import Data.Proxy (Proxy(..))
import Data.Data (Data, Constr, DataType, toConstr, constrType, dataTypeOf, dataTypeName, dataTypeConstrs)

-- newtype PartialType a = PartialType (AllSet Data.Set.Set Constr)
--     deriving (Show, Eq, Monoid, BaseContainer, Constructible, Collapseable, Intersectable, Container, ContainerMappable)
type PartialType a = AllSet Data.Set.Set Constr
type PartialType' = AllSet Data.Set.Set Constr

instance Ord Constr where       -- Required for Data.Set
    compare a b = compare (dataTypeName $ constrType a) (dataTypeName $ constrType b)
        `mappend` compare (show a) (show b) -- I don't like using "show" here, but we don't have anything else (it should be "constring")

----------------------------------------------------------------------------------------------------

basicType :: Proxy a -> PartialType a
basicType _ = All

fullAlgebraicType :: forall a. (Data a) => Proxy a -> PartialType a
fullAlgebraicType _ = fromList $ dataTypeConstrs $ dataTypeOf (undefined :: a)

partialAlgebraicType :: forall a. (Data a) => a -> PartialType a
partialAlgebraicType proxy = singleton $ toConstr proxy

isOfType :: forall a. (Data a) => a -> PartialType a -> Bool
isOfType value typ = typ `contains` toConstr value

-- class Eq t => PartialType t where
--     contains :: t -> Constr -> Bool
--     disjunct :: t -> t -> Bool
--     union :: t -> t -> t

----------------------------------------------------------------------------------------------------

-- data PseudoType a
--     = Any
--     | Constrs (Set Constr)
--     deriving (Show, Eq)

-- instance PartialType (PseudoType a) where
--     contains :: PseudoType a -> Constr -> Bool
--     contains (Any) _ = True
--     contains (Constrs set) = 
--     disjunct :: PseudoType a -> PseudoType a -> Bool
--     disjunct 