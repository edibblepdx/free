-- coproducts of functors
--
-- for any monad `f`, we can inject it into a free monad over a functor sum

module Sum where

import Free

-- The sum of two functors
data (f :+: g) a = InL (f a) | InR (g a)

-- Is itself a functor
instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (InL e) = InL (fmap f e)
  fmap f (InR e) = InR (fmap f e)

-- The type constraint f :<: g is satisfied
-- if there is some injection from f to g.
class (Functor f, Functor g) => f :<: g where
  inject :: f a -> g a

-- :<: is reflexive.
instance (Functor f) => f :<: f where
  inject = id

-- To inject any value of type `f a` into
-- a type `(f :+: g) a` is equivalent to InL.
instance (Functor f, Functor g) => f :<: (f :+: g) where
  inject = InL

-- To inject any value of type `f a` into
-- a type `(g :+: f) a` is equivalent to InR.
instance (Functor f, Functor g) => f :<: (g :+: f) where
  inject = InR

inj :: (Functor f, Functor g, f :<: g) => Free f a -> Free g a
inj (Pure x) = Pure x
inj (Free x) = Free $ inject (fmap inj x)

injF :: (Functor f, f :<: g) => f a -> Free g a
injF = liftF . inject
