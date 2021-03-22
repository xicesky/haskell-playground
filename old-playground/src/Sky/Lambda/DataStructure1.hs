
-- Those are required only for the Eq and Show instances of DataFix
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

-- Required for correctly typing the lambda data structures
{-# LANGUAGE RankNTypes #-}

-- Required to declare Show and Eq on lambda data structures
-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE FlexibleInstances #-}

module Sky.Lambda.DataStructure1 where

-- There is a way to represent ADTs in standard lambda calculus.

-- Take for example the following Haskell data definitions:

-- The product of two data types is a tuple
data DataProd a b = DataProd { _xpa :: a, _xpb :: b }
  deriving (Eq, Show)

-- The sum of two data types is an "Either"
data DataSum a b = DataSumA { _xsa :: a } | DataSumB { _xsb :: b }
  deriving (Eq, Show)

-- Void
data DataVoid = DataVoid

-- A recursive data structure can be built using a fixpoint function
data DataFix f = DataFix { _xl :: f (DataFix f) }
  --deriving (Eq, Show)

instance Show (f (DataFix f)) => Show (DataFix f) where
  show x = "(" ++ show (_xl x) ++ ")"
  
instance Eq (f (DataFix f)) => Eq (DataFix f) where
  (==) a b = _xl a == _xl b

----------------------------------------------------------------------------------------------------
-- Example usage

-- Example data
exampleSimple :: DataProd Int Bool
exampleSimple = DataProd 5 True

-- Similar to: data L a b = Cons a b | Nil
-- FIXME: Why the fuck is there a tuple in there, we have DataProd!
data DataLElem a rec = DataLElem (DataSum (a, rec) ())  -- Should just be a type synonym, but haskell won't allow it
  deriving (Eq, Show)

type DataList a = DataFix (DataLElem a)

-- Little helpers
dh :: DataSum (a, DataList a) () -> DataList a
dh = DataFix . DataLElem

dnil :: DataList a
dnil = dh $ DataSumB ()

dcons :: a -> DataList a -> DataList a
dcons x xs = dh $ DataSumA (x, xs)

exampleRec :: DataList Int
exampleRec = dcons 1 $ dcons 2 $ dcons 3 $ dnil
--exampleRec = DataFix $ DataSumA $ (1, DataFix $ DataSumA $ (2, DataFix $ DataSumB () ))

exToList :: DataList Int -> [Int]
exToList (DataFix (DataLElem (DataSumB ()))) = []
exToList (DataFix (DataLElem (DataSumA (x, xs)))) = x : (exToList xs)



----------------------------------------------------------------------------------------------------
-- So now here we go with simple lambda

-- DataProd becomes:
type LProd a b = forall x. (a -> b -> x) -> x

-- Sorry i didn't get this to work
--instance (c ~ String, Show a, Show b) => Show ((a -> b -> String) -> String) where
--  show lp = lp $ \a b -> "(_lprod " ++ show a ++ " " ++ show b ++ ")"

_lprod :: a -> b -> LProd a b
_lprod a b op = op a b

_lpa :: LProd a b -> a
_lpa d = d $ \a b -> a

_lpb :: LProd a b -> b
_lpb d = d $ \a b -> b

-- DataSum becomes:
type LSum a b = forall x. (a -> x) -> (b -> x) -> x

_lsuma :: a -> LSum a b
_lsuma a opa opb = opa a

_lsumb :: b -> LSum a b
_lsumb b opa opb = opb b

_lsa :: LSum a b -> a
_lsa d = d (\a -> a) (\b -> error "No A in LSumB") -- Similar to what haskell does

_lsb :: LSum a b -> b
_lsb d = d (\a -> error "No B in LSumA") (\b -> b) -- Similar to what haskell does

-- DataVoid becomes:
type LVoid = forall a. a -> a

_lvoid :: LVoid     -- constructor
_lvoid = id

_lv :: LVoid -> ()  -- deconstructor
_lv v = v ()

-- DataFix becomes:
--type LFix f = LSum _ rec -> LFix 

--_lfix :: LHolds a -> 
--_lfix = sum 

-- DataFix is simply the normal fixpoint

-- We would like to the define it as:
--    kind LFix :: (* -> *) -> *
--    type LFix f = f (LFix f)
-- Which is not possible, because type synonyms cannot by cyclic, so
-- we have to use a newtype here

--newtype LFix f = LFix (f (LFix f))

--_lfix :: (a -> a) -> LFix ()
--_lfix f = let rec = f rec in LFix rec

--_lfix :: (a -> a) -> a
--_lfix f = let x = f x in x

----------------------------------------------------------------------------------------------------
-- We can now substitute the data constructors simply via application :)

lProdToTuple :: LProd a b -> (a, b)
lProdToTuple d = d $ (,)

-- a funny duality:
-- Instead of taking two functions, LSum could take a pair of functions
type LPSum a b = forall x. LProd (a -> x) (b -> x) -> x

mkLPSum :: LSum a b -> LPSum a b
mkLPSum s fpair = s (_lpa fpair) (_lpb fpair)

----------------------------------------------------------------------------------------------------
-- Translation of the examples above

-- Simple data
lExampleSimple :: LProd Int Bool
lExampleSimple = _lprod 5 True


-- -- Similar to: data L a b = Cons a b | Nil
-- -- FIXME: Why the fuck is there a tuple in there, we have DataProd!
-- data DataLElem a rec = DataLElem (DataSum (a, rec) ())  -- Should just be a type synonym, but haskell won't allow it
--   deriving (Eq, Show)

type LElem a rec = LSum (LProd a rec) LVoid

-- type DataList a = DataFix (DataLElem a)
--type LList a = LFix (LElem a)
newtype LList a = LList (LElem a (LList a))

-- -- Little helpers
-- dh :: DataSum (a, DataList a) () -> DataList a
-- dh = DataFix . DataLElem

-- dnil :: DataList a
-- dnil = dh $ DataSumB ()

-- dcons :: a -> DataList a -> DataList a
-- dcons x xs = dh $ DataSumA (x, xs)

-- exampleRec :: DataList Int
-- exampleRec = dcons 1 $ dcons 2 $ dcons 3 $ dnil
-- --exampleRec = DataFix $ DataSumA $ (1, DataFix $ DataSumA $ (2, DataFix $ DataSumB () ))

-- exToList :: DataList Int -> [Int]
-- exToList (DataFix (DataLElem (DataSumB ()))) = []
-- exToList (DataFix (DataLElem (DataSumA (x, xs)))) = x : (exToList xs)
