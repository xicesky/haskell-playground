
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Source: http://hackage.haskell.org/package/logict-0.7.1.0/docs/Control-Monad-Logic.html
module NonDetSearch.LogicT where

import Control.Applicative
import Control.Monad
import Control.Monad.Logic
import NonDet.Class

{- Notes:
LogicT uses the common "carrier" type:
    forall r. (a -> m r -> m r) -> m r -> m r

This is the same as the "HaskellWiki" implementation. But it
provides a lot of utilities, especially the "MonadLogic" class!

LogicT has MonadFail & MonadPlus instances that provide
    fail _ = mzero = empty
-}

{-----------------------------------------------------------------------------}
-- Logic demo

parents :: [ (String, String) ]
parents =
    [ ("Sarah",  "John")
    , ("Arnold", "John")
    , ("John",   "Anne")
    ]

grandparent :: String -> Logic String
grandparent grandchild = do
    (p, c) <- anyof parents
    (c', g) <- anyof parents
    guard (c == c')
    guard (g == grandchild)
    pure p

{-
>>> observeAll (grandparent "Anne")
-}

{-----------------------------------------------------------------------------}
-- BFS

-- we need a seperate type for discerning search order
newtype LogicBFS a = LogicBFS
    { unLogicBFS :: Logic a
    }
    deriving (Functor, Applicative, MonadFail, MonadLogic)
    -- deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadFail, MonadLogic)

instance Alternative LogicBFS where
    empty = LogicBFS empty

    -- Use interleave instead of <|>
    (<|>) a b = LogicBFS $ unLogicBFS a `interleave` unLogicBFS b
    -- (<|>) a b = LogicBFS $ unLogicBFS a <|> unLogicBFS b

instance Monad LogicBFS where
    return = pure

    -- Use >>- instead of >>=
    (>>=) a b = LogicBFS $ unLogicBFS a >>- unLogicBFS . b
    -- (>>=) a b = LogicBFS $ unLogicBFS a >>= unLogicBFS . b

instance MonadPlus LogicBFS

{-----------------------------------------------------------------------------}
-- NonDet interface

instance NonDet (LogicT m)

searchLogicT :: SFun
searchLogicT = SFun search where
    search :: Logic a -> [a]
    search = observeAll

instance NonDet LogicBFS

searchLogicBFS :: SFun
searchLogicBFS = SFun search where
    search :: LogicBFS a -> [a]
    search = observeAll . unLogicBFS
