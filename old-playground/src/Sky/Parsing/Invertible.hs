
{-# LANGUAGE InstanceSigs #-}               -- Because i love it
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}                 -- See syntaxIso

module Sky.Parsing.Invertible where

import Prelude hiding (id, (.), (<$>), (<*>), pure, print)
import Control.Category                     -- yay
import Control.Monad (liftM2, mplus)

import Data.Functor.Identity
import Text.Read (readMaybe)
import Data.Maybe (fromJust)

import Sky.Isomorphism.Simple

----------------------------------------------------------------------------------------------------
-- Doesn't work with the van Laarhoven SemiIso: f (SemiIso' ...) requires ImpredicativeTypes
-- We are using the simpler version to have an instance of Category.
type Iso a b = MumuIso Maybe a b

-- Semi-Isomorphisms on lists
nil :: Iso () [a]
nil = iso (\_ -> return []) toNil where
    toNil [] = Just ()
    toNil _  = Nothing
cons :: Iso (a, [a]) [a]
cons = iso doCons headTail where
    doCons (x, xs) = Just (x : xs)
    headTail [] = Nothing
    headTail (x : xs) = Just (x, xs)

many :: forall p a. (Syntax p) => p a -> p [a]
many p =    nil <$> pure ()
        <|> cons <$> p <*> many p

-- Semi-Isomorphism for specific characters
onlyChar :: Char -> Iso Char Char
onlyChar c = iso check check where
    check input = if input == c
        then Just c
        else Nothing

----------------------------------------------------------------------------------------------------
{-  A haskell functor is covariant:
        fmap :: forall f a b. (Functor f) => (a -> b) -> f a -> f b

    This works for parsers, e.g.:
        newtype Parser a = Parser { _parse :: String -> a }
        instance Functor Parser where
            fmap :: forall a b. (a -> b) -> Parser a -> Parser b
            fmap f (Parser p) = Parser (f . p)
-}

{-  Pretty-printers on the other hand are contravariant:
        cofmap :: forall f a b. (Contravariant f) => (a -> b) -> f b -> f a

    E.g.:
        newtype Printer a = Printer { _print :: a -> String }
        instance Contravariant Printer where
            contramap :: forall a b. (a -> b) -> Printer b -> Printer a
            contramap f (Printer p) = Printer (p . f)
-}

{-  In order to get both, we can simply apply an isomorphism instead of a function:
-}

class IsoFunctor f where
    isomap :: forall a b. Iso a b -> f a -> f b

{-  We could also make this a profunctor... if it really had 2 arguments. -}

infixl 5 <$>
(<$>) :: forall f a b. (IsoFunctor f) => Iso a b -> f a -> f b
(<$>) = isomap

--infixl 4 <*>
--class IsoFunctor f => IsoApplicative f where
--    (<*>) :: forall a b. f (Iso a b) -> f a -> f b

class ProductFunctor f where
    productF :: forall a b. f a -> f b -> f (a, b)

infixl 6 <*>
(<*>) :: forall f a b. (ProductFunctor f) => f a -> f b -> f (a, b)
(<*>) = productF

class Alternative f where
    emptyF :: forall a. f a
    choiceF :: forall a. f a -> f a -> f a

infixl 4 <|>
(<|>) :: forall f a. (Alternative f) => f a -> f a -> f a
(<|>) = choiceF

class (IsoFunctor f, ProductFunctor f, Alternative f) => Syntax f where
    pure :: forall a. (Eq a) => a -> f a
    token :: f Char

----------------------------------------------------------------------------------------------------
-- Demo syntax

--demoSyntax :: Parser [[Char]]
demoSyntax :: Syntax s => s [[Char]]
demoSyntax = many syntaxAB where
    syntaxAB = cons <$> syntaxA <*> (cons <$> syntaxB <*> (nil <$> pure ()))
    syntaxA = onlyChar 'a' <$> token
    syntaxB = onlyChar 'b' <$> token

----------------------------------------------------------------------------------------------------
-- Parser

---- Simple parser / pp from read / show
--mkInvertible :: forall a. (Read a, Show a) => Iso String a
--mkInvertible = iso readMaybe (return . show)

newtype Parser a = Parser { _parse :: String -> [(a, String)] }
parse :: Parser a -> String -> [a]
parse (Parser p) s = [ x | (x, "") <- p s ]

parse1 :: Parser a -> String -> Maybe a
parse1 p s = case parse p s of
    []      -> Nothing
    (x : _) -> Just x

instance IsoFunctor Parser where
    isomap :: forall a b. Iso a b -> Parser a -> Parser b
    isomap iso (Parser p) = Parser q where
        q s =   [ (y, rest)
                | (x, rest) <- p s              -- Parse using p
                , Just y <- [apply iso x]       -- Apply isomorphism to results x
                ]

instance ProductFunctor Parser where
    productF :: forall a b. Parser a -> Parser b -> Parser (a, b)
    productF (Parser pa) (Parser pb) = Parser p where
        p s =   [ ((x,y), rest2)
                | (x, rest1) <- pa s            -- Parse using pa
                , (y, rest2) <- pb rest1        -- Parse rest using pb
                ]

instance Alternative Parser where
    emptyF :: forall a. Parser a
    emptyF = Parser (\_ -> [])                  -- Parse nothing (fail)
    choiceF :: forall a. Parser a -> Parser a -> Parser a
    choiceF (Parser pa) (Parser pb) = Parser p where
        p s =   pa s ++ pb s                    -- Simply find all results from either pa or pb

instance Syntax Parser where
    pure :: forall a. (Eq a) => a -> Parser a
    pure a = Parser (\s -> [(a, s)])            -- Don't parse, just generate a
    token :: Parser Char
    token = Parser p where
        p [] = []                               -- No more input
        p (x : xs) = [(x, xs)]                  -- Accept x

-- Lets test it
demoParse = _parse demoSyntax "ababcd"  where -- parse demoParser "abab" where

----------------------------------------------------------------------------------------------------
-- Prettyprinter

newtype Printer a = Printer { _print :: a -> Maybe String }

print :: Printer a -> a -> Maybe String
print = _print
-- print (Printer p) x = p x

instance IsoFunctor Printer where
    isomap :: forall a b. Iso a b -> Printer a -> Printer b
    isomap iso (Printer p) = Printer q where
        q v =   unapply iso v                   -- First "fetch" the value using iso
                >>= p                           -- Then print

instance ProductFunctor Printer where
    productF :: forall a b. Printer a -> Printer b -> Printer (a, b)
    productF (Printer pa) (Printer pb) = Printer p where
        p (va, vb) = liftM2 (++) (pa va) (pb vb)

instance Alternative Printer where
    emptyF :: forall a. Printer a
    emptyF = Printer (\_ -> Nothing)            -- Just fail
    choiceF :: forall a. Printer a -> Printer a -> Printer a
    choiceF (Printer pa) (Printer pb) = Printer p where
        p v = mplus (pa v) (pb v)               -- Print using pa, or pb if pa fails
            {- case pa v of
            Just result -> Just result
            Nothing -> pb v -}

instance Syntax Printer where
    pure :: forall a. (Eq a) => a -> Printer a
    pure v = Printer p where
        p value = if value == v     -- Check
            then Just ""            -- but don't print
            else Nothing
    token :: Printer Char
    token = Printer p where
        p v = Just [v]              -- Print it

-- Lets test it
demoPrint = _print demoSyntax ["ab","ab"]  where

----------------------------------------------------------------------------------------------------
-- Parser and Prettyprinter are an isomorphism!

syntaxIso :: forall a. (forall s. (Syntax s) => s a) -> Iso String a
syntaxIso s = iso (parse1 s) (print s)

syntaxIsoDemo :: IO ()
syntaxIsoDemo = do
    demo <- return $ syntaxIso demoSyntax
    parsed <- return $ apply demo "ababab"
    putStrLn $ "Parsed:     " ++ show parsed
    printed <- return $ unapply demo (fromJust parsed)
    putStrLn $ "Printed:    " ++ show printed
    return ()
