
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE MultiWayIf             #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
--{-# LANGUAGE GADTs                  #-}

{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE TypeOperators          #-}
--{-# LANGUAGE MultiParamTypeClasses  #-}
--{-# LANGUAGE TypeSynonymInstances #-}

--{-# LANGUAGE DataKinds              #-}
--{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE KindSignatures         #-}

{-    Haskell types sometimes are not "precise" enough (e.g. ADTs)
    and we lack the power to check dynamically which types a function
    expects.
-}

module Sky.Ideas.Types where

import Prelude hiding (lookup, (!!), (<), (>), (<=), (>=))
import qualified Data.Ord as Ord
import Data.Void
import Data.Proxy
import Data.Typeable
import Data.Data
import GHC.Generics

import Control.Monad (join)
import Sky.Util.NewContainer    -- TreeSet
import Data.Set (isSubsetOf)
import Data.List (intersperse)

type Set a = TreeSet a          -- Cannot use HashSet because it has no Ord instance

----------------------------------------------------------------------------------------------------
-- Sadly, Haskell does not provide a class for partial orders (!)

class Eq a => PartialOrd a where
    (<=) :: a -> a -> Bool
    (>=) :: a -> a -> Bool
    (>=) = flip (<=)
    (<) :: a -> a -> Bool
    a < b = a <= b && (a /= b)
    (>) :: a -> a -> Bool
    (>) = flip (<)

    compare :: a -> a -> Maybe Ord.Ordering
    compare a a' = if
        | a == a'   -> Just Ord.EQ
        | a <= a'   -> Just Ord.LT
        | a >= a'   -> Just Ord.GT
        | otherwise -> Nothing
    -- Efficiency?
    --compare a b = if a <= b
    --    then (if a == b then Just Ord.EQ else Just Ord.LT)
    --    else (if b <= a then Just Ord.GT else Nothing)

instance PartialOrd Int where
    (<=) = (Ord.<=)

----------------------------------------------------------------------------------------------------

-- When i say "type" really mean types and type constructors of arbitrary arity.

type TypeConstructor = (TypeRep, String)
showSTypeConstructor :: TypeConstructor -> ShowS
showSTypeConstructor (_, name) = showString name

data TypePrimitive
    = HaskellType TypeRep
    | IsoType TypeConstructor Type
    deriving (Typeable, Generic, Eq, Ord)
    -- Ord and Hashable are required for sets
--instance Hashable TypePrimitive

data TypeProduct    = TypeProduct { fromTypeProduct :: [TypePrimitive] }
    deriving (Typeable, Generic, Eq, Ord)
    -- Ord and Hashable are required for sets
--instance Hashable TypeProduct
--instance Eq TypeProduct where -- A product is equal only if the types are in the same order

data TypeSum        = TypeSum (Set TypeProduct)
    deriving (Typeable, Generic, Eq, Ord)
--instance Hashable TypeSum

type Type = TypeSum

{-  -- Old definition
    data Type
        = HaskellType TypeRep
        | IsoType TypeConstructor Type
        | ProductType Type Type
        | SumType Type Type
        | Empty
        deriving (Show)
-}

haskellTypePrimitive :: Typeable a => Proxy a -> Type
haskellTypePrimitive proxy = TypeSum . singleton . TypeProduct . singleton . HaskellType $ h where
    h = typeRep proxy

isoType :: TypeConstructor -> Type -> Type
isoType tyCon (TypeSum ts) = TypeSum . cmap mkIso $ ts where
    mkIso :: TypeProduct -> TypeProduct
    mkIso = TypeProduct . singleton . IsoType tyCon . TypeSum . singleton

typeSum :: Type -> Type -> Type
typeSum (TypeSum setA) (TypeSum setB) = TypeSum (union setA setB)

typeProduct :: Type -> Type -> Type
typeProduct (TypeSum as) (TypeSum bs) = TypeSum $ fromList $ [prod a b | a <- toList as, b <- toList bs] where
    prod (TypeProduct a) (TypeProduct b) = TypeProduct $ a ++ b

----------------------------------------------------------------------------------------------------
-- Show / Read

showsListF :: (a -> ShowS) -> ShowS -> [a] -> ShowS
showsListF single seperator = foldl (.) id . intersperse seperator . fmap single
{-
showsListF single seperator []      s = s
showsListF single seperator (x:xs)  s = single x . seperator . showsListF single seperator xs
-}

class AType t where
    --parseType :: String -> Type   -- Problem with TypeRep
    showsPrecType :: Int -> t -> ShowS
    -- =
    showType :: t -> String
    showType t = showsPrecType 0 t ""
    extractSums :: t -> Set t       -- Normalization
    -- =
    --xHaskellTypes :: t -> [TypeRep]
    --getHaskellType :: t -> TypeRep
    -- =

instance AType TypePrimitive where
    showsPrecType prec (HaskellType t)      = showsPrec 0 t
    showsPrecType prec (IsoType tyCon t)    = showParen (prec > 2) $ showSTypeConstructor tyCon . showString " " . showsPrecType 3 t
    extractSums t@(HaskellType _) = singleton t
    extractSums (IsoType con t) = cmap (IsoType con) (extractSums t)
    --getHaskellType (HaskellType t)          = t
    --getHaskellType (IsoType con t)          = fst con   -- FIXME
    --xHaskellTypes (HaskellType t)           = [t]
    --xHaskellTypes (IsoType (tyCon,_) t)     = 

instance AType TypeProduct where
    showsPrecType prec (TypeProduct [t])    = showsPrec prec t
    showsPrecType prec (TypeProduct ts)     = showParen True $ showsListF (showsPrecType 2) (showString ", ") ts where
    extractSums (TypeProduct []) = singleton $ TypeProduct []
    extractSums (TypeProduct (x:xs)) = fromList $ do  -- List Monad
        a <- toList $ extractSums x
        b <- toList $ extractSums (TypeProduct xs)
        return $ TypeProduct $ a : (fromTypeProduct b)
    --xHaskellTypes (TypeProduct ts)          = fmap haskellType ts

    --getHaskellType (TypeProduct ts)         = 

instance AType TypeSum where
    showsPrecType prec (TypeSum tset)       = show' tlist where
        tlist :: [TypeProduct]
        tlist = toList tset
        show' :: [TypeProduct] -> ShowS
        show' []    = showString "Empty"
        show' [t]   = showsPrecType prec t
        show' ts    = showParen (prec > 0) $ showsListF (showsPrecType 1) (showString " | ") ts
    extractSums (TypeSum set) = let
        extracted :: [[TypeProduct]]
        extracted = fmap (toList . extractSums) . toList $ set
        in fromList . fmap (TypeSum . singleton) . join $ extracted
    --haskellType (TypeSum set) = ht (toList set) where
    --    ht []       = typeRep[Void]
    --    ht (x:xs)   = checkType (haskellType x) xs
    --    checkType h []                          = h
    --    checkType h (x:xs)  | h = haskellType x = checkType h xs
    --    checkType h (x:xs)                      = error $ "Haskell types " ++ show h ++ " and " ++ show haskellType x ++ " are different."
    --    {-
    --    ht (x:xs)   = if all (== haskellType x) xs
    --            then haskellType x
    --            else error "Different Haskell Types"
    --    -}


{-
-- Undecidable :(
instance AType t => Show t where
    show = showType
-}

instance Show TypePrimitive where
    show = showType

instance Show TypeProduct where
    show = showType

instance Show TypeSum where
    show = showType

----------------------------------------------------------------------------------------------------
-- Normalization

--normalize :: Type -> Type
--normalize (TypeSum set) = undefined where

----------------------------------------------------------------------------------------------------
-- Types form a partial order
-- They MUST BE NORMALIZED in order for this to work

instance PartialOrd TypePrimitive where
    HaskellType a <= HaskellType b          = a == b  -- FIXME: Not accurate in general
    IsoType n1 t1 <= IsoType n2 t2          = (n1 == n2) && (t1 <= t2)

instance PartialOrd x => PartialOrd [x] where
    [] <= []            = True
    (x:xs) <= []        = False
    [] <= (y:ys)        = False
    (x:xs) <= (y:ys)    = (x <= y) && (xs <= ys)

instance PartialOrd TypeProduct where
    -- When both types are normalized, this should just be (==) for all factors...
    TypeProduct as <= TypeProduct bs        = as <= bs

instance PartialOrd TypeSum where
    -- When both types are normalized, this is simple:
    TypeSum as <= TypeSum bs                = as `isSubsetOf` bs

--haskellType :: Type -> TypeRep
--haskellType (HaskellType typeRep) = typeRep

----------------------------------------------------------------------------------------------------

type TMetaInfo = TypeRep
getMeta :: Typeable a => Proxy a -> TMetaInfo
getMeta = typeRep
getMetaV :: forall a. Typeable a => a -> TMetaInfo
getMetaV _ = getMeta (Proxy :: Proxy a)

metaConstructor :: forall t c (f :: * -> *) a. Constructor c => TMetaInfo -> Proxy (t c f a) -> TypeConstructor
metaConstructor mi _ = (mi, conName (undefined :: t c f a))

----------------------------------------------------------------------------------------------------

class GPrint f where
    g_Constructor :: TMetaInfo -> f a -> TypeConstructor
    -- =
    g_AllConstructors :: TMetaInfo -> f a -> [TypeConstructor]
    -- =
    g_ConstructorName :: f a -> String
    -- =
    g_AllConstructorNames :: f a -> [String]
    -- =
    g_FindStrictestType :: TMetaInfo -> f a -> Type
    -- =
    g_FindSimpleType :: TMetaInfo -> f a -> Type
    -- =

instance GPrint U1 where
    g_Constructor _ U1 = error "U1"
    g_AllConstructors _ _ = error "U1"
    g_ConstructorName U1 = error "U1"
    g_AllConstructorNames _ = error "U1"
    g_FindStrictestType _ U1 = haskellTypePrimitive (Proxy :: Proxy ())
    g_FindSimpleType _ _ = haskellTypePrimitive (Proxy :: Proxy ())

instance Typeable a => GPrint (K1 i a) where
    g_Constructor _ (K1 x) = error "K1"
    g_AllConstructors _ _ = error "K1"
    g_ConstructorName :: K1 i a p -> String
    g_ConstructorName (K1 x) = error "K1"
    g_AllConstructorNames _ = error "K1"
    g_FindStrictestType _ (K1 x) = haskellTypePrimitive (Proxy :: Proxy a)
    g_FindSimpleType _ _ = haskellTypePrimitive (Proxy :: Proxy a)

instance (GPrint a, GPrint b) => GPrint (a :*: b) where
    g_Constructor _ (a :*: b) = error ":*:"
    g_AllConstructors _ _ = error ":*:"
    g_ConstructorName :: (a :*: b) p -> String
    g_ConstructorName (a :*: b) = error ":*:"
    g_AllConstructorNames _ = error ":*:"
    g_FindStrictestType mi (a :*: b) = typeProduct (g_FindStrictestType mi a) (g_FindStrictestType mi b)
    g_FindSimpleType mi _ = typeProduct (g_FindSimpleType mi (undefined :: a p)) (g_FindSimpleType mi (undefined :: b p))

instance (GPrint a, GPrint b) => GPrint (a :+: b) where
    g_Constructor mi (L1 x) = g_Constructor mi x
    g_Constructor mi (R1 x) = g_Constructor mi x
    g_AllConstructors mi _ = g_AllConstructors mi (undefined :: a p) ++ g_AllConstructors mi (undefined :: b p)
    g_ConstructorName :: (a :+: b) p -> String
    g_ConstructorName (L1 x) = g_ConstructorName x
    g_ConstructorName (R1 x) = g_ConstructorName x
    g_AllConstructorNames _ = g_AllConstructorNames (undefined :: a p) ++ g_AllConstructorNames (undefined :: b p)
    g_FindStrictestType mi (L1 x) = g_FindStrictestType mi x
    g_FindStrictestType mi (R1 x) = g_FindStrictestType mi x
    g_FindSimpleType mi _ = typeSum (g_FindSimpleType mi (undefined :: a p)) (g_FindSimpleType mi (undefined :: b p))

instance (GPrint a) => GPrint (D1 c a) where
    g_Constructor mi (M1 x) = g_Constructor mi x
    g_AllConstructors mi _ = g_AllConstructors mi (undefined :: a p)
    g_ConstructorName :: D1 c a p -> String
    g_ConstructorName (M1 x) = g_ConstructorName x
    g_AllConstructorNames _ = g_AllConstructorNames (undefined :: a p)
    g_FindStrictestType mi (M1 x) = g_FindStrictestType mi x
    g_FindSimpleType mi _ = g_FindSimpleType mi (undefined :: a p)

instance (Constructor c, GPrint a) => GPrint (C1 c a) where
    g_Constructor mi c = metaConstructor mi (Proxy :: Proxy (C1 c a p))
    g_AllConstructors mi _ = [metaConstructor mi (Proxy :: Proxy (C1 c a p))]
    g_ConstructorName :: C1 c a p -> String
    g_ConstructorName = conName
    g_AllConstructorNames _ = [conName (undefined :: C1 c a p)]
    g_FindStrictestType mi c@(M1 x) = isoType (g_Constructor mi c) (g_FindStrictestType mi x)
    g_FindSimpleType mi _ = isoType (g_Constructor mi (undefined :: C1 c a p)) (g_FindSimpleType mi (undefined :: a p))

instance (GPrint a) => GPrint (S1 c a) where
    g_Constructor mi (M1 x) = g_Constructor mi x
    g_AllConstructors mi _ = g_AllConstructors mi (undefined :: a p)
    g_ConstructorName :: S1 c a p -> String
    g_ConstructorName (M1 x) = g_ConstructorName x
    g_AllConstructorNames _ = g_AllConstructorNames (undefined :: a p)
    g_FindStrictestType mi (M1 x) = g_FindStrictestType mi x
    g_FindSimpleType mi _ = g_FindSimpleType mi (undefined :: a p)

constructorName :: (Generic a, GPrint (Rep a)) => a -> String
constructorName a = g_ConstructorName (from a)

allConstructorNames :: (Generic a, GPrint (Rep a)) => a -> [String]
allConstructorNames a = g_AllConstructorNames (from a)

findStrictestType :: (Typeable a, Generic a, GPrint (Rep a)) => a -> Type
findStrictestType a = g_FindStrictestType (getMetaV a) (from a)

findSimpleType :: forall a. (Typeable a, Generic a, GPrint (Rep a)) => Proxy a -> Type
findSimpleType proxy = g_FindSimpleType (getMeta proxy) (undefined :: Rep a x)

----------------------------------------------------------------------------------------------------
-- Examples for interactive testing

data TVoid
    deriving (Typeable, Generic)

data Example
    = CExample1 Int
    | CExample2 Bool
    deriving (Typeable, Data, Generic)

class Blubb a where
    say :: a -> String
    say _ = "Hello"

--instance Blubb (CExample1 Int)
