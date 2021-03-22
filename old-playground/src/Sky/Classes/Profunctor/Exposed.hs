
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Sky.Classes.Profunctor.Exposed where

import Sky.Classes.Profunctor

class (Monad (ExposedM p), Profunctor p) => ExposedProfunctor p where
    type ExposedM p :: * -> *
    expose :: p a b -> p ((ExposedM p) a) b
    merge  :: p a ((ExposedM p) b) -> p a b 
