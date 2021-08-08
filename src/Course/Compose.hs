{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Compose where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.Monad
import Course.Contravariant

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a =
  Compose (f (g a)) deriving (Show, Eq)

-- Implement a Functor instance for Compose
instance (Functor f, Functor g) =>
    Functor (Compose f g) where
  aToB <$> c@(Compose fga) = Compose ((aToB <$>) <$> fga)

instance (Applicative f, Applicative g) =>
  Applicative (Compose f g) where
-- Implement the pure function for an Applicative instance for Compose
  pure a = Compose (pure (pure a))
-- Implement the (<*>) function for an Applicative instance for Compose
  x@(Compose fgAToB) <*> y@(Compose fga) = Compose $ lift2 (<*>) fgAToB fga

instance (Monad f, Monad g) =>
  Monad (Compose f g) where
-- Implement the (=<<) function for a Monad instance for Compose
  x =<< y = error "i cannae do it cap'n; she's impossible!"

-- Note that the inner g is Contravariant but the outer f is
-- Functor. We would not be able to write an instance if both were
-- Contravariant; why not?                  f           g
-- Compose                            :: (a -> b) -> (b -> c) -> a -> c 
-- Compose with Contravariant f       :: (b -> a) -> (b -> c) -> a -> c
-- Compose with Contravariant g       :: (a -> b) -> (c -> b) -> a -> c 
-- Compose with Contravariant g and f :: (b -> a) -> (c -> b) -> a -> c impossible b/c 
instance (Functor f, Contravariant g) =>
  Contravariant (Compose f g) where
-- Implement the (>$<) function for a Contravariant instance for Compose
  bToA >$< (Compose comp) = Compose $ (bToA >$<) <$> comp

-- contramap:
-- If you have a context that needs an a (comp :: Compose f g a)
-- and a function that can convert bs to as, (bToA :: b -> a)
-- I can give you a context that needs bs. (Compose f g b)