-- Free monads

module Free where

data Free f a = Pure a | Free (f (Free f a))

instance (Functor f) => Functor (Free f) where
  fmap g (Pure a) = Pure (g a)
  fmap g (Free fa) = Free (fmap g <$> fa)

instance (Functor f) => Applicative (Free f) where
  pure = Pure
  Pure a <*> Pure b = Pure $ a b
  Pure a <*> Free mb = Free $ fmap a <$> mb
  Free ma <*> b = Free $ (<*> b) <$> ma

instance (Functor f) => Monad (Free f) where
  -- return = Pure
  Pure a >>= f = f a
  Free m >>= f = Free ((>>= f) <$> m)

liftF :: (Functor f) => f a -> Free f a
liftF = Free . fmap Pure
