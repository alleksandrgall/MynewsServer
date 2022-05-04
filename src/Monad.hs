module Monad where

class Functor' f where
  fmap' :: (a -> b) -> f a -> f b

class Applicative' f where
  pure' :: a -> f a
  (<*>>>) :: f (a -> b) -> f a -> f b

applMap :: Applicative' f => (a -> b) -> f a -> f b
applMap f t = pure' f <*>>> t

class Monad' m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b