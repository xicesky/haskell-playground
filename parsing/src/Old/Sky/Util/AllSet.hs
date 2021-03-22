
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}

module Old.Sky.Util.AllSet where

import Prelude hiding (lookup, (!!))
import Old.Sky.Util.NewContainer

----------------------------------------------------------------------------------------------------
-- Definiton

-- Isomorphic to "Either () (Set a)" or "Maybe (Set a)"
data AllSet s (v :: *)
    = All
    | JustSet (s v)

----------------------------------------------------------------------------------------------------
-- Base instances

instance (Show v, ElemT (s v) ~ v, Collapseable (s v)) => Show (AllSet s v) where
    showsPrec p (All)           = showString "All"
    showsPrec p (JustSet set)   = showParen (p > 10) $
        showString "fromList " . shows (toList set)

instance (Eq (s v)) => Eq (AllSet s v) where
    All         == All          = True
    All         == (JustSet _)  = False
    (JustSet _) == All          = False
    (JustSet a) == (JustSet b)  = a == b

-- TODO
-- instance (Data (s v)) => Data (AllSet s v) where
-- instance (NFData (s v)) => NFData (AllSet s v) where

instance (Semigroup (s v)) => Semigroup (AllSet s v) where
    -- Union
    (<>) All _ = All
    (<>) _ All = All
    (<>) (JustSet a) (JustSet b) = JustSet ((<>) a b)

instance (Monoid (s v)) => Monoid (AllSet s v) where
    -- Empty container
    mempty = JustSet mempty
    -- Union
    mappend = (<>)

----------------------------------------------------------------------------------------------------
-- Container instances

instance BaseContainer (AllSet s v) where
    type ElemT (AllSet s v) = v

instance (Constructible (s v), ElemT (s v) ~ v) => Constructible (AllSet s v) where
    empty = JustSet empty
    insert v (JustSet s) = JustSet (insert v s)
    singleton = JustSet . singleton
    fromList = JustSet . fromList

instance (Collapseable (s v), ElemT (s v) ~ v) => Collapseable (AllSet s v) where
    -- FIXME: This does not look right: Both "All" and "Empty" collapse to the empty list
    c_foldMap map (All) = mempty
    c_foldMap map (JustSet set) = c_foldMap map set
    elements (All) = []
    elements (JustSet set) = elements set
    size (All) = 0
    size (JustSet set) = size set
    toAscList (All) = []
    toAscList (JustSet set) = toAscList set

instance (Collapseable (s v), Intersectable (s v), ElemT (s v) ~ v) => Intersectable (AllSet s v) where
    union All _ = All
    union _ All = All
    union (JustSet a) (JustSet b) = JustSet (union a b)

    intersection All r = r
    intersection r All = r
    intersection (JustSet a) (JustSet b) = JustSet (intersection a b)

    disjunct All (JustSet b) = isEmpty b
    disjunct (JustSet a) All = isEmpty a
    disjunct (JustSet a) (JustSet b) = disjunct a b

-- instance (Diffable (s v), ElemT (s v) ~ v) => Diffable (AllSet s v) where
--     difference _ All = empty
--     difference All (JustSet set) = undefined    -- PROBLEMATIC!
--     difference (JustSet a) (JustSet b) = JustSet (difference a b)

instance (Container (s v), ElemT (s v) ~ v) => Container (AllSet s v) where
    contains All v = True
    contains (JustSet set) v = contains set v

instance (ContainerMappable (s a) (s b), ElemT (s a) ~ a, ElemT (s b) ~ b) => ContainerMappable (AllSet s a) (AllSet s b) where
    cmap f All              = All
    cmap f (JustSet set)    = JustSet (cmap f set)
