
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE DeriveGeneric          #-} -- Hashable

module Sky.Parsing.Invertible2 where

import Prelude hiding (lookup, (!!), print, (.), id)

import Control.Category
import Control.Applicative (liftA2)

import Data.Functor.Identity
import Control.Monad (join)
import Data.List (sort, intercalate)

-- Hashable
import GHC.Generics (Generic)

-- Debug stuff
import Debug.Trace
import Data.Function ((&))

import Sky.Classes.Isomorphism.Monomorphic
import Sky.Implementations.Isomorphism
import Sky.Implementations.Isomorphism.MonoIso
import Sky.Util.NewContainer
--import Sky.Util.NewGraphLike
import Sky.Util.GraphMonad

----------------------------------------------------------------------------------------------------
-- Shortcuts

class (Show a, Typeable a, Eq a, Ord a, Hashable a) => D a
instance D Char
instance D a => D [a]
-- where -- Sublime Text fucks up Haskell Syntax

----------------------------------------------------------------------------------------------------
-- Token recognition

data TokenSet token
    = AnyToken
    | SomeTokens (HashSet token)
    | SingleToken token
    deriving (Show, Eq, Generic)
instance Hashable token => Hashable (TokenSet token)
--instance (Show token, Eq token, Hashable token) => D (TokenSet token)

-- More like regex, sets and sequences of tokens...

tokenSet :: (Ord a, Hashable a) => [a] -> TokenSet a
tokenSet = SomeTokens . fromList

----------------------------------------------------------------------------------------------------
-- Syntax

data Syntax' token a where
    Empty       :: Syntax' token a                                                      -- Fail
    Pure        :: a -> Syntax' token a                                                 -- Generate a without input
    Token       :: Iso token a -> TokenSet token -> Syntax' token a                     -- Accept a certain set of tokens
    Sequence    :: (D x, D y) => Iso (x,y) a -> Syntax' token x -> Syntax' token y -> Syntax' token a           -- Sequence
    Alternative :: (D x, D y) => Iso (Either x y) a -> Syntax' token x -> Syntax' token y -> Syntax' token a    -- Alternative
    ReferenceTo :: (D x) => Iso x a -> Reference String (Syntax' token x) -> Syntax' token a                    -- Call to a reference

-- Our "Syntax" monad
type SyntaxM token a = GraphT String Identity (Syntax' token a)

--letRec :: (Monad m, RefName k, Typeable v) => k -> (Reference k v -> v) -> GraphT k m (Reference k v)

define :: (Typeable token, D a) => String -> (Syntax' token a -> SyntaxM token a) -> SyntaxM token a
define name inner = fmap (ReferenceTo id) $ letRec name (\ref -> inner (ReferenceTo id ref))

define' :: (Typeable token, D a) => String -> Syntax' token a -> SyntaxM token a
define' name = define name . const . return

----------------------------------------------------------------------------------------------------
-- Utilities for Syntax

applyIso :: Iso a b -> Syntax' token a -> Syntax' token b
applyIso f (Empty) = Empty
applyIso f (Pure a) = Pure $ apply f a
applyIso f (Token g recg) = Token (f . g) recg
applyIso f (Sequence g a b) = Sequence (f . g) a b
applyIso f (Alternative g a b) = Alternative (f . g) a b
applyIso f (ReferenceTo g r) = ReferenceTo (f . g) r -- error $ "Cannot apply ismorphism to reference: " ++ getReferenceName r

lit :: (Ord token, Hashable token) => token -> Syntax' token token
lit a = Token (iso id id) (SingleToken a)

many :: (Typeable token, D a) => String -> Syntax' token a -> SyntaxM token [a]
many name single = define name $ \recMany -> return $
    Alternative (isoAlt null) (Pure []) $ Sequence isoCons single recMany

litLeft :: (D token, D a) => token -> Syntax' token a -> Syntax' token a
litLeft x y = Sequence (isoFixedLeft x) (lit x) y

litRight :: (D token, D a) => Syntax' token a -> token -> Syntax' token a
litRight x y = Sequence (isoFixedRight y) x (lit y)

between :: (D token, D a) => token -> token -> Syntax' token a -> Syntax' token a
between x1 x2 y = x1 `litLeft` (y `litRight` x2)

opNonAssoc :: (D token, D a, D b) => String -> Iso (a,a) b -> Syntax' token a -> token -> SyntaxM token b
opNonAssoc name isoOp simple op = define' name $ Sequence isoOp simple $ litLeft op simple

opLeftAssoc :: (D token, D a, D b, D e) => String -> Iso (e,a) b -> Iso (Either b a) e -> Syntax' token a -> token -> SyntaxM token e
opLeftAssoc name isoOp isoDecide simple op =    -- LR: AddExpr = AddExpr Op Simple | Simple
        define name $ \expr ->
        return $ Alternative isoDecide (Sequence isoOp expr $ litLeft op simple) simple

opAnyAssoc :: (D token, D a, D b, D e) => String -> Iso (e,e) b -> Iso (Either b a) e -> Syntax' token a -> token -> SyntaxM token e
opAnyAssoc name isoOp isoDecide simple op =     -- LR: AddExpr = AddExpr Op AddExpr | Simple
        define name $ \expr ->
        return $ Alternative isoDecide (Sequence isoOp expr $ litLeft op expr) simple

----------------------------------------------------------------------------------------------------
-- More specific utilities

parens :: forall ast a. (D a) => Syntax' Char a -> Syntax' Char a
parens = between '(' ')'

digit :: Syntax' Char Char
digit = Token id $ tokenSet "0123456789"

number :: SyntaxM Char DemoAST
number = do
    digits <- many "Digits" digit
    return $ applyIso (iso toLiteral fromLiteral) digits
    where
        toLiteral :: String -> DemoAST
        toLiteral = Literal . read
        fromLiteral :: DemoAST -> String
        fromLiteral (Literal i) = show i

----------------------------------------------------------------------------------------------------
-- AST

data DemoAST
    = Literal Int
    | Add DemoAST DemoAST
    -- | Variable String
    deriving (Show, Eq, Ord, Generic)
instance Hashable DemoAST
instance D DemoAST

-- Isomorphisms for AST

isoAdd :: Iso (DemoAST, DemoAST) DemoAST
isoAdd = iso (\(a,b) -> Add a b) (\(Add a b) -> (a, b)) -- (\x -> traceShow x $ x & \(Add a b) -> (a, b))

isoIsLiteral :: Iso (Either DemoAST DemoAST) DemoAST
isoIsLiteral = isoAlt isLiteral where
    isLiteral :: DemoAST -> Bool
    isLiteral (Literal _) = True
    isLiteral _           = False

isoIsAdd :: Iso (Either DemoAST DemoAST) DemoAST
isoIsAdd = isoAlt isAdd where
    isAdd :: DemoAST -> Bool
    isAdd (Add _ _) = True
    isAdd _         = False

----------------------------------------------------------------------------------------------------
-- "Pretty" printer

print :: forall token a. (Typeable token, D a) => Syntax' token a -> a -> GraphT String Identity (Maybe [token])
print (Empty)                 _   = return Nothing -- & trace "Empty"
print (Token iso _)           ast = return $ Just $ [unapply iso ast] -- & trace "Token"
print (Pure ast2)             ast = return $ if (ast == ast2) then Just [] else Nothing -- & trace "Pure"
print (Sequence iso sa sb)    ast = do
        (a, b) <- return $ unapply iso ast -- & trace "Sequence"
        -- (liftA2.liftA2) (++) (print sa a) (print sb b)   -- short (unreadable) version
        pa <- print sa a
        pb <- print sb b
        return $ (++) <$> pa <*> pb     -- Combine two "Maybe String" values using (++)
print (Alternative iso sa sb) ast = case unapply iso ast -- & trace "Alternative"
        of
        Left x -> print sa x
        Right y -> print sb y
print (ReferenceTo iso r)     ast = do
        ast' <- return $ unapply iso ast
        real <- resolveReference r
        print real $ ast'
            & trace ("Reference " ++ getReferenceName r ++ ": " ++ show ast')

print' :: (Typeable token, D a) => SyntaxM token a -> a -> Maybe [token]
print' syntax expr = runIdentity $ evalGraphT $ do
    entry <- syntax
    print entry expr

----------------------------------------------------------------------------------------------------

demoSyntax :: SyntaxM Char DemoAST
demoSyntax = define "Expr" $ \expr -> do
    literal <- number
    simple <- define' "Simple" $ Alternative isoIsLiteral literal (parens expr)
    --addExpr <- opNonAssoc "AddExpr" isoAdd simple '+'
    addExpr <- opAnyAssoc "AddExpr" isoAdd isoIsAdd simple '+'
    return $ addExpr -- Alternative isoIsAdd addExpr simple

demoAST :: DemoAST
demoAST = (Literal 1 `Add` Literal 2) `Add` (Literal 3 `Add` Literal 4)

demoString :: String
demoString = "2+(3+4)"

--grammar1 :: (D token) => Syntax' token ast a -> String
--grammar1 (Empty)             = "(Fail)"
--grammar1 (Token _ toks)       = case toks of
--    AnyToken        -> "."
--    SomeTokens set  -> intercalate "|" $ fmap show $ sort $ toList set
--    SingleToken tok -> show tok
--grammar1 (Pure _)            = "(Pure)"
--grammar1 (Sequence _ a b)    = grammar1 a ++ " " ++ grammar1 b
--grammar1 (Alternative _ a b) = grammar1 a ++ " | " ++ grammar1 b
--grammar1 (Reference name)    = name

--grammar :: forall token ast. (D token, D ast) => Syntax token ast -> String
--grammar syn = join $ map rule $ toList syn where
--    rule :: (String, Syntax' token ast a) -> String
--    rule (name, g) = name ++ " = " ++ grammar1 g ++ "\n"
