
-- Using evil ListT
{-# OPTIONS_GHC -Wno-deprecations #-}

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DefaultSignatures #-}

{- We want to be able to watch the solver go, but
also influence it by:
    - overriding choices
    - cancelling / delaying decisions
-}
module NonDet.Interactive where

import Text.Printf
--import Data.Proxy
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.List
import NonDet.Class

import Debug.Trace

{-----------------------------------------------------------------------------}

class NonDet m => Interactive a b m | m -> a, m -> b
    where
    iYield :: a -> m b
    -- iTrace :: String -> m ()

{-----------------------------------------------------------------------------}
-- General support via coroutines

data CRStatus a b m r
    = Done r
    | Suspended a (b -> m (CRStatus a b m r))

newtype CR a b m r = CR
    {   resume :: m (CRStatus a b m r)
    }

instance Monad m => Functor (CR a b m) where
    fmap = liftM

-- FIXME can this constraint be relaxed?
instance Monad m => Applicative (CR a b m) where
    pure = return
    (<*>) = ap

instance Monad m => Monad (CR x y m) where
    return :: r -> CR x y m r
    return = CR . return . Done
    (>>=) :: CR x y m a -> (a -> CR x y m b) -> CR x y m b
    (>>=) ma amb = CR $ resume ma >>= apply amb where
        apply :: (a -> CR x y m b) -> CRStatus x y m a -> m (CRStatus x y m b)
        apply amb' (Done a)         = resume (amb' a)
        apply amb' (Suspended x fy) = return $ Suspended x $
            fy >=> apply amb'

instance MonadPlus m => Alternative (CR x y m) where
    empty = CR empty
    (<|>) (CR a) (CR b) = CR $ a <|> b

instance MonadPlus m => MonadPlus (CR x y m)

instance NonDet m => NonDet (CR x y m) where
    -- Try to make choices a bit more efficient
    choices = CR . choices . fmap resume

instance MonadTrans (CR x y) where
    lift = CR . fmap Done

instance MonadIO m => MonadIO (CR x y m) where
    liftIO = lift . liftIO

yield :: Monad m => a -> CR a b m b
yield x = CR $ return $ Suspended x $ return . Done

instance NonDet m => Interactive x y (CR x y m) where
    iYield = yield

runCR :: forall a b m r. Monad m => (a -> m b) -> CR a b m r -> m r
runCR f cr = resume cr >>= loop where
    loop :: CRStatus a b m r -> m r
    loop (Done r) = return r
    loop (Suspended x cont) = f x >>= cont >>= loop

crDemo :: IO ()
crDemo = runCR askUser demoRoutine where
    askUser :: Int -> IO Bool
    askUser i = putStrLn ("Got: " ++ show i) >> loop where
        loop = do
            putStr "Continue? [ (y)es ] / (n)o : "
            getLine >>= \case
                []      -> return True
                'y':_   -> return True
                'n':_   -> return False
                _       -> loop
    demoRoutine :: CR Int Bool IO ()
    demoRoutine = loop 0 where
        loop i = yield i >>= \case
            True -> loop (i+1)
            False -> return ()

{-----------------------------------------------------------------------------}
-- List Monad + CR

instance Monad m => NonDet (ListT m)

newtype InListT x y m a = InListT { unInListT :: CR x y (ListT m) a }
    deriving (Functor, Applicative, Monad, Alternative, MonadPlus, NonDet)

deriving instance Monad m => Interactive x y (InListT x y m)
deriving instance MonadIO m => MonadIO (InListT x y m)

-- runCR :: forall a b m r. Monad m => (a -> m b) -> CR a b m r -> m r
runInListT :: Monad m => (a -> ListT m b) -> InListT a b m r -> m [r]
runInListT f = runListT . runCR f . unInListT

{-----------------------------------------------------------------------------}
-- Demo

data PGTrace
    = ChooseHole Int Int [Int]
    | FailedGuard String
    deriving (Show, Eq, Ord)

-- FIXME: There should be a class or something
pgGuard :: Interactive PGTrace () m => String -> Bool -> m ()
pgGuard _ True = pure ()
pgGuard msg False = iYield (FailedGuard msg) >> failure

-- Pidgeonhole, n into m
interPidgeonHole :: Interactive PGTrace () m => Int -> Int -> m [Int]
interPidgeonHole 0 _ = return []
interPidgeonHole n m = do
    traceM $ "choosing pidgeon holes " ++ show n ++ " -> " ++ show m
    others <- interPidgeonHole (n-1) m

    -- Choose hole for n-th pidgeon
    hole <- anyof [1..m]

    -- Report chosen hole
    iYield (ChooseHole n hole others)

    -- Hole must be free
    pgGuard ("Hole " ++ show hole ++ " is already taken") $
        notElem hole others

    return $ hole : others

pgDemo :: IO ()
pgDemo = do
    result <- runInListT askUser $ interPidgeonHole 3 2
    liftIO $ print result
    where
    askUser :: (MonadIO m, NonDet m) => PGTrace -> m ()
    askUser action = do
        liftIO (showAction action)
        case action of
            ChooseHole {}   -> askPrune
            _               -> return ()

    askPrune :: (MonadIO m, NonDet m) => m ()
    askPrune = do
        liftIO $ putStr "Explore this? [ (y)es ] / (n)o : "
        liftIO getLine >>= \case
            []      -> return ()
            'y':_   -> return ()
            'n':_   -> empty
            _       -> askPrune

    showAction :: PGTrace -> IO ()
    showAction (ChooseHole pidgeon hole others) = do
        -- putStrLn $ "Got: " ++ show i
        printf "Pidgeon #%d gets hole #%d. (already assigned: %s)\n"
            pidgeon hole (show others)
    showAction (FailedGuard msg) = do
        putStrLn msg
