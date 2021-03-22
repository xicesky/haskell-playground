
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}           -- Required for handling SemiIsos (Non type-variable argument in the constraint)
{-# LANGUAGE RankNTypes #-}                 -- Since we redefine apply and unapply

module Sky.Isomorphism.SemiIsoDemo where

-- TODO: Use Sky.Isomorphism.Class

import Data.Functor.Identity
import Data.Char (toUpper, toLower)
import Control.Monad.Except

import Sky.Isomorphism.SemiIso

apply :: forall m s t a b. Monad m => SemiIso m s t a b -> s -> m a
apply = applySemiIso
unapply :: forall m s t a b. Monad m => SemiIso m s t a b -> b -> m t
unapply = unapplySemiIso

-- Let's do a simple semi-isomorphism:
-- Transform strings between "all lowercase" and "all uppercase"
upperIso :: SemiIso' Identity String String
upperIso = semiIso toUpperStr toLowerStr where
    toUpperStr :: String -> Identity String
    toUpperStr = return . (fmap toUpper)
    toLowerStr :: String -> Identity String
    toLowerStr = return . (fmap toLower)

-- Now let's do one that can fail:
-- This isomorphism fails on "_"
upperIsoFail :: SemiIso' Maybe String String
upperIsoFail = semiIso toUpperStr toLowerStr where
    checkUnderscore :: (Char -> Char) -> Char -> Maybe Char
    checkUnderscore f '_' = Nothing
    checkUnderscore f x = Just (f x)

    toUpperStr :: String -> Maybe String
    toUpperStr = traverse (checkUnderscore toUpper)

    toLowerStr :: String -> Maybe String
    toLowerStr = traverse (checkUnderscore toLower)

-- Maybe you'd like an error message instead?
upperIsoErr :: SemiIso' (Either String) String String
upperIsoErr = semiIso toUpperStr toLowerStr where
    checkUnderscore :: (Char -> Char) -> Char -> Either String Char
    checkUnderscore f '_'   = Left "Encountered underscore"
    checkUnderscore f x     = Right (f x)

    toUpperStr :: String -> (Either String) String
    toUpperStr = traverse (checkUnderscore toUpper)

    toLowerStr :: String -> (Either String) String
    toLowerStr = traverse (checkUnderscore toLower)

-- We can build a nice one between "Maybe a" and "[a]" to have something
-- that actually changes types. Also we will be using the "Except" monad
-- instead of "Either" because it handles better
list1iso :: forall a. (Show a) => SemiIso' (Except String) (Maybe a) [a]
list1iso = semiIso maybeToList listToMaybe where
    maybeToList :: Maybe a -> Except String [a]
    maybeToList (Nothing)   = return []
    maybeToList (Just x)    = return [x]
    listToMaybe :: [a] -> Except String (Maybe a)
    listToMaybe []          = return Nothing
    listToMaybe [x]         = return (Just x)
    listToMaybe xs          = throwError $ "List of multiple elements: " ++ show xs

demo :: IO ()
demo = do
    putStrLn $ "apply upperIso \"abcDEF\"                         = " ++ show (apply upperIso "abcDEF")
    putStrLn $ "unapply upperIso \"abcDEF\"                       = " ++ show (unapply upperIso "abcDEF")
    putStrLn $ "apply (reverseIso upperIso) \"abcDEF\"            = " ++ show (apply (reverseIso upperIso) "abcDEF")
    putStrLn ""
    putStrLn $ "apply upperIsoFail \"abcDEF\"                     = " ++ show (apply upperIsoFail "abcDEF")
    putStrLn $ "apply upperIsoFail \"abc_DEF\"                    = " ++ show (apply upperIsoFail "abc_DEF")
    putStrLn $ "unapply upperIsoFail \"abc_DEF\"                  = " ++ show (unapply upperIsoFail "abc_DEF")
    putStrLn ""
    putStrLn $ "apply upperIsoErr \"abcDEF\"                      = " ++ show (apply upperIsoErr "abcDEF")
    putStrLn $ "apply upperIsoErr \"abc_DEF\"                     = " ++ show (apply upperIsoErr "abc_DEF")
    putStrLn $ "unapply upperIsoErr \"abc_DEF\"                   = " ++ show (unapply upperIsoErr "abc_DEF")
    putStrLn ""
    putStrLn $ "apply list1iso (Nothing)                        = " ++ show (runExcept $ apply list1iso (Nothing :: Maybe Char))
    putStrLn $ "apply list1iso (Just 'x')                       = " ++ show (runExcept $ apply list1iso (Just 'x'))
    putStrLn $ "unapply list1iso ([])                           = " ++ show (runExcept $ unapply list1iso ([] :: [Char]))
    putStrLn $ "unapply list1iso (['y'])                        = " ++ show (runExcept $ unapply list1iso (['y']))
    putStrLn $ "unapply list1iso (['a','b','c'])                = " ++ show (runExcept $ unapply list1iso (['a','b','c']))
