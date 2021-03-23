
{-# LANGUAGE InstanceSigs #-}               -- Because i love it
{-# LANGUAGE ScopedTypeVariables #-}

module Ideas.Old.ShowType where

-- Shows type of an argument
-- Does not work for polymorphic types of course!

class ShowType t where
    showType :: t -> String
    printType :: t -> IO ()
    printType = putStrLn . showType

instance ShowType () where
    showType _ = "()"

instance ShowType Bool where
    showType _ = "Bool"

instance ShowType Int where
    showType _ = "Int"

instance (ShowType a, ShowType b) => ShowType (a -> b) where
    showType _ = showType a ++ " -> " ++ showType b where
        a = undefined :: a
        b = undefined :: b
