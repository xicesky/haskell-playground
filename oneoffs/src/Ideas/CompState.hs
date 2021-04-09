
{-# LANGUAGE AllowAmbiguousTypes #-}

-- Compositional state monad without algebraic effects
module Ideas.CompState where

import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Optics
import Optics.State
import Optics.Zoom

{--------------------------------------------------------------------------------------------------}

{-
Various ideas for composing state:

- Use heterogenous lists and indices

- Use something like open product types.
    Compdata has something but uses it only for "annotations".
    Could take inspiration from open unions by okmij:
        http://okmij.org/ftp/Haskell/extensible/#open-union

- Just define it as "something having a lens" (first try implemented below)

-}

{--------------------------------------------------------------------------------------------------}
-- Using lens

{-
Basic idea:
Instead of fixing (a, b) as product of a and b, we just define the projection class, similar to
the subsumption in compdata, which gives us a "Lens p a" for any type p that has a "factor" a.

Operator for 'factors'? (:|:)
-}
class Factor s a where
    -- We could define 'inject' and 'project' (set and get)
    -- inject :: a -> s -> s
    -- project :: s -> a

    -- Or just a lens
    factor :: Lens' s a

{--------------------------------------------------------------------------------------------------}

-- Then, if we want to provide some functionality that requires some state:
newtype Counter = CCounter { _counter :: Int }
    deriving (Show, Eq)

{- The 10000th hls crash
https://github.com/haskell/haskell-language-server/issues/1297
https://github.com/haskell/haskell-language-server/issues/1342
-}
--makeLenses ''Counter

counter :: Iso' Counter Int
counter = iso (\ (CCounter x) -> x) CCounter
{-# INLINE counter #-}

incCounter :: (MonadState s m, Factor s Counter) => m Int
incCounter = do
    i <- use $ factor % counter
    modifying (factor % counter) (+1)
    return i

newtype Storage = CStorage { _storage :: String }

storage :: Iso' Storage String
storage = iso (\ (CStorage x) -> x) CStorage
{-# INLINE storage #-}

checkStorage :: (MonadState s m, Factor s Storage) => m Int
checkStorage = do
    s <- use $ factor % storage
    return $ length s

setStorage :: (MonadState s m, Factor s Storage) => String -> m String
setStorage s = do
    s0 <- use $ factor % storage
    assign (factor % storage) s
    return s0

{--------------------------------------------------------------------------------------------------}

{- And the disadvantages?
- No automatic impl
- Type errors can get out of hand sometimes
- We can't initialize the state via a simple "run" method
- Nested states need more and more boilerplate declarations (see below)
- Indexed state types require weird type applications (see below)
    because they require AllowAmbiguousTypes or Proxies
-}

data MyState = CMyState
    { msCounter :: Counter
    , msStorage :: Storage
    }

instance Factor MyState Counter where
    factor = lens msCounter (\s v -> s {msCounter=v})
instance Factor MyState Storage where
    factor = lens msStorage (\s v -> s {msStorage=v})

-- Demo:

lensfulDemo :: IO ()
lensfulDemo = do
    _ <- runStateT demo $ CMyState (CCounter 0) (CStorage "")
    return ()
    where
    demo :: StateT MyState IO ()
    demo = do
        replicateM_ 5 $ do
            i <- incCounter
            liftIO $ print i
        x <- checkStorage
        liftIO $ print x

        _ <- setStorage "hello"
        y <- setStorage "world"
        liftIO $ print y
        z <- setStorage "..."
        liftIO $ print z

        return ()

{--------------------------------------------------------------------------------------------------}
-- Nested state boilerplate

data MyNested1 = CMyNested1
    { mn1Counter :: Counter
    , mn1Other :: Int
    }

instance Factor MyNested1 Counter where
    factor = lens mn1Counter (\s v -> s {mn1Counter=v})

data MyNested2 = CMyNested2
    { mn2Nested1 :: MyNested1
    , mn2Other :: Int
    }

instance Factor MyNested2 MyNested1 where
    factor = lens mn2Nested1 (\s v -> s {mn2Nested1=v})

instance Factor MyNested2 Counter where
    factor = factor % factor @MyNested1

{--------------------------------------------------------------------------------------------------}
-- Indexed state types are weird

newtype IState n = CIState { _iState :: [n] }

iState :: Iso' (IState n) [n]
iState = iso (\ (CIState x) -> x) CIState
{-# INLINE iState #-}

countElems :: forall n s m. (MonadState s m, Factor s (IState n)) => m Int
countElems = do
    xs <- use $ factor @s @(IState n) % iState
    return $ length xs

data MyState3 n = CMyState3
    { ms2Counter :: Counter
    , ms2Storage :: IState n
    }

instance Factor (MyState3 n) Counter where
    factor = lens ms2Counter (\s v -> s {ms2Counter=v})
instance Factor (MyState3 n) (IState n) where
    factor = lens ms2Storage (\s v -> s {ms2Storage=v})

indexedStateDemo :: IO ()
indexedStateDemo = do
    _ <- runStateT demo $ CMyState3 (CCounter 0) (CIState ["a", "b", "c"])
    return ()
    where
    demo :: StateT (MyState3 String) IO ()
    demo = do
        i <- countElems @String
        liftIO $ print i
