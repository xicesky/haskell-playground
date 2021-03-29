
{-# LANGUAGE EmptyDataDeriving #-}

module Ideas.UpdateDS where

import Data.Kind (Type)
import Data.Proxy
import Data.Maybe
import Data.Monoid (Endo(..))
import Data.Functor.Const

{--------------------------------------------------------------------------------------------------}
{-
Given a datastructure @D@, represent partial updates without structural change using a datastructure
@U@, where @U@ is a monoid.
-}

data Example a b
    = NotAThing
    | SomeA a
    | SomeB b
    | SomeRec a b (Example a b)

data ExampleU a b
    = USomeA (a -> a)
    | USomeB (b -> b)
    | USomeRec (Maybe (a -> a)) (Maybe (b -> b)) (Maybe (ExampleU a b))

updateEx :: forall a b. ExampleU a b -> Example a b -> Example a b
updateEx (USomeA f) (SomeA a) = SomeA (f a)
updateEx (USomeB f) (SomeB b) = SomeB (f b)
updateEx (USomeRec mf mg urec) (SomeRec a b r) = let
    newA :: a
    newA = fromMaybe id mf a
    newB :: b
    newB = fromMaybe id mg b
    newR :: Example a b
    newR = maybe r (`updateEx` r) urec
    --mf <*> pure
    in SomeRec newA newB newR
updateEx _ _ = error "Structure mismatch (updateEx)"

{-
Law: (like Endo)
    update (a <> b) = update a . update b
-}

instance (Semigroup a, Semigroup b) => Semigroup (ExampleU a b) where
    USomeRec ma0 mb0 mr0 <> USomeRec ma1 mb1 mr1
        {- FIXME This is definitely wrong, because it combines functions f <> g pointwise
        (f a <> g a) instead of composing them (f . g) - it should probably just use 'Endo'
        -}
        = USomeRec (ma0 <> ma1) (mb0 <> mb1) (mr0 <> mr1)
    USomeA f0 <> USomeA f1 = USomeA (f0 . f1)
    USomeB f0 <> USomeB f1 = USomeB (f0 . f1)
    _ <> _ = error "Structure mismatch (<>)"

{--------------------------------------------------------------------------------------------------}
-- Generalization

type Location a = Diff (Const Bool) a

newtype ME a = ME (Maybe (Endo a))
type Update a = Diff ME a

applyME :: ME a -> a -> a
applyME (ME me) = maybe id appEndo me

{- Here is a nice idea: Why not
    (?) Diff Identity a =~ a
-}

-- FIXME how to add Monoid (Diff f a) ?
class (Eq (Location a)) => HasUpdate a where
    type Diff (f :: Type -> Type) a :: Type
    update :: Update a -> a -> a
    toLocation :: Proxy a -> Update a -> Location a
    -- = --

instance HasUpdate (a, b, c) where
    -- Just put the f EVERYWHERE. fmapping functors over types?
    type Diff f (a, b, c) = (f a, f b, f c)
    -- This looks awfully familiar (like parallel traversal)
    update (mfa, mfb, mfc) (a, b, c) = (applyME mfa a, applyME mfb b, applyME mfc c)
    toLocation _ (ME mfa, ME mfb, ME mfc) =
        (Const $ isJust mfa, Const $ isJust mfb, Const $ isJust mfc)

exNotifyChange :: Show a => a -> IO ()
exNotifyChange a = putStrLn $ "Changed to: " ++ show a

liftU :: Monad m => (a -> m ()) -> (Maybe a -> m ())
liftU f Nothing     = return ()
liftU f (Just a)    = f a

-- Example: Tuples (a, b, c) where a < b, c > 0

{--------------------------------------------------------------------------------------------------}

data Problem a
    deriving (Eq, Ord, Functor)

instance Applicative Problem where
    pure _ = undefined
    _ <*> _ = undefined

instance Monad Problem where
    return = undefined
    _ >>= _ = undefined

data Variable a
data Constraint

var :: Problem (Variable a)
var = undefined

constrain :: Constraint -> m ()
constrain = undefined

check :: Bool -> Constraint
check = undefined

instance Eq a => Eq (Variable a) where
    (==) = undefined

instance Ord a => Ord (Variable a) where
    compare = undefined

-- This can't really work, just for demo syntax rn
instance Num a => Num (Variable a) where
    (+) = undefined
    (*) = undefined
    abs = undefined
    signum = undefined
    fromInteger = undefined
    negate = undefined

problem :: Problem (Variable Int, Variable Int, Variable Int)
problem = do
    a <- var
    b <- var
    c <- var
    constrain $ check $ a < b
    constrain $ check $ c > 0
    return (a, b, c)
