
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE BangPatterns           #-}

module Sky.Parsing.Invertible3.Isomorphism where

import Prelude hiding (id, (.))
import Data.Data (Data)
import Data.Proxy (Proxy(..))
import Control.Category (Category(..))

import Sky.Parsing.Invertible3.PartialType

-- import Data.Function ((&))
-- import Debug.Trace

----------------------------------------------------------------------------------------------------

-- class IsoError e where
--     errorComposition :: Iso e b c -> Iso e a b -> e
--     showError :: e -> String

class Category i => Isomorphism i where
    apply :: i s a -> s -> a
    unapply :: i s a -> a -> s
    reverseIso :: i s a -> i a s
    addIso :: (Data s) => i s a -> i s b -> i s (Either a b)

----------------------------------------------------------------------------------------------------
-- Implementation: Errors

data IsoError
    = CompositionError
        { codomainLeft :: PartialType'
        , domainRight :: PartialType'
        }
    | IsoAdditionError
        { domain1 :: PartialType'
        , domain2 :: PartialType'
        }

-- instance IsoError DefaultIsoError where
--     errorComposition isoBC isoAB = CompositionError (_codomain isoAB) (_domain isoBC)
--     showError (CompositionError codom dom) = "Iso composition domain mismatch: " ++ show codom ++ " vs " ++ show dom

errorComposition :: Iso b c -> Iso a b -> IsoError
errorComposition isoBC isoAB = CompositionError (_codomain isoAB) (_domain isoBC)

errorAddition :: Iso s a -> Iso s b -> IsoError
errorAddition isoS1A isoS2B = IsoAdditionError (_domain isoS1A) (_domain isoS2B)
--err = error $ "Iso sum domain clash: " ++ show (_domain isoS1A) ++ " vs " ++ show (_domain isoS2B)

showError :: IsoError -> String
showError (CompositionError codom dom) =
    "Isomorphisms mismatch on composition:\n"
    ++ "    The codomain of the first isomorphism:\n"
    ++ "        " ++ show codom ++ "\n"
    ++ "    doesn't match the domain of the second:\n"
    ++ "        " ++ show dom ++ "\n"
showError (IsoAdditionError dom1 dom2) =
    "Isomorphisms mismatch on addition:\n"
    ++ "    The domain of the first isomorphism:\n"
    ++ "        " ++ show dom1 ++ "\n"
    ++ "    is not disjunct from the the domain of the second:\n"
    ++ "        " ++ show dom2 ++ "\n"

--throwError :: IsoError e => e -> a
throwError :: IsoError -> a
throwError e = error $ showError e

----------------------------------------------------------------------------------------------------
-- Implementation: Iso

data Iso s a
    = Iso
        { _rawIso :: (s -> a, a -> s)
        , _domain :: PartialType s
        , _codomain :: PartialType a
        }
    | Error IsoError

instance Category (Iso) where
    id :: Iso a a
    id = iso id id

    (.) :: Iso b c -> Iso a b -> Iso a c
    (.) (Error e) _ = Error e
    (.) _ (Error e) = Error e
    (.) !isoBC !isoAB = if check then isoAC else err where
        check = _codomain isoAB == _domain isoBC
            -- & ( trace $ "Check " ++ (show $ _codomain isoAB) ++ " == " ++ (show $ _domain isoBC) )
        err = Error $ errorComposition isoBC isoAB
        (bmc, cmb) = _rawIso isoBC
        (amb, bma) = _rawIso isoAB
        amc = bmc . amb
        cma = bma . cmb
        isoAC = Iso (amc, cma) (_domain isoAB) (_codomain isoBC)

instance Isomorphism (Iso) where
    apply (Error e) = throwError e
    apply (Iso (forward, _) _ _) = forward
    
    unapply (Error e) = throwError e
    unapply (Iso (_, backward) _ _) = backward
    
    reverseIso (Error e) = Error e
    reverseIso (Iso (f, b) d c) = Iso (b, f) c d

    addIso :: forall s a b. (Data s) => Iso s a -> Iso s b -> Iso s (Either a b)
    addIso  (Error e) _ = Error e
    addIso  _ (Error e) = Error e
    addIso isoS1A isoS2B = if check then Iso (forward, backward) newDomain newCodomain else err where
        check = disjunct (_domain isoS1A) (_domain isoS2B)
        err = Error $ errorAddition isoS1A isoS2B
        newDomain = (_domain isoS1A) `union` (_domain isoS2B)
        newCodomain = basicType (Proxy :: Proxy (Either a b))

        forward :: s -> Either a b
        forward v = if v `isOfType` _domain isoS1A
            then Left $ apply isoS1A v
            else Right $ apply isoS2B v

        backward :: Either a b -> s
        backward (Left b1)  = unapply isoS1A b1
        backward (Right b2) = unapply isoS2B b2

----------------------------------------------------------------------------------------------------

-- Simple total iso
iso :: (s -> a) -> (a -> s) -> Iso s a
iso forward backward = Iso (forward, backward) (basicType (Proxy :: Proxy s)) (basicType (Proxy :: Proxy a))

-- Construction from a data constructor: Partial on the left data type (s)
partialIso :: forall e s a. (Data s) => s -> (s -> a) -> (a -> s) -> Iso s a
partialIso dConstr forward backward = Iso (forward, backward) (partialAlgebraicType dConstr) (basicType (Proxy :: Proxy a))

-- Construction from a data constructor: Partial on the right data type (s)
partialIsoR :: forall e s a. (Data a) => a -> (s -> a) -> (a -> s) -> Iso s a
partialIsoR dConstr forward backward = Iso (forward, backward) (basicType (Proxy :: Proxy s)) (partialAlgebraicType dConstr)
--partialIsoR dConstr forward backward = reverseIso $ partialIso dConstr backward forward

----------------------------------------------------------------------------------------------------
-- For invertible syntax parsing we need:

-- A "failure" isomorphism
-- TODO: The "parser" side should not accept anything, this still has to compose (e.g. with alt)
isoFail :: String -> Iso a b
isoFail e = iso (error e) (error e)

-- An isomorphism between fixed values (i.e. "pure" & "token" for the parser)
isoPure :: s -> a -> Iso s a
isoPure s a = iso (const a) (const s)

-- Alternative choice: Having parsed either "s" or "t", generate "a"
-- this is just "addIso" for the partial isos above! (reversed, in this case)
isoAlt :: (Data a) => Iso s a -> Iso t a -> Iso (Either s t) a
isoAlt sa ta = reverseIso $ addIso (reverseIso sa) (reverseIso ta)

-- Sequential combination
-- TODO: Parse side needs to be able to make a choice (for determinism)
isoSeq :: Iso [s] a -> Iso [s] b -> Iso [s] (a, b)
isoSeq lsa lsb = error $ "TODO"

-- Because it happens a lot and haskell would "do it wrong":
isoSeqL :: Iso [s] a -> Iso [s] () -> Iso [s] a
isoSeqL lsa lsb = error $ "TODO"
isoSeqR :: Iso [s] () -> Iso [s] b -> Iso [s] b
isoSeqR lsa lsb = error $ "TODO"

----------------------------------------------------------------------------------------------------
-- Debug

checkIso :: Iso a b -> Iso a b
checkIso (Error e)  = throwError e
checkIso iso        = iso

instance Show (Iso a b) where
    show (Error e) = showError e
    show _ = "Iso (FIXME)"
