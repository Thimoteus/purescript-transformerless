module Control.Monad.Transformerless.Reader where

import Base

newtype Reader r a = Reader (r -> a)

runReader :: forall r a. Reader r a -> r -> a
runReader (Reader f) = f

withReader :: forall r1 r2 a. (r2 -> r1) -> Reader r1 a -> Reader r2 a
withReader f (Reader r1) = Reader (r1 <<< f)

mapReader :: forall r a b. (a -> b) -> Reader r a -> Reader r b
mapReader f (Reader a) = Reader (f <<< a)

instance functorReader :: Functor (Reader r) where
  map = mapReader

instance applyReader :: Apply (Reader r) where
  apply (Reader f) (Reader a) = Reader \ r -> f r (a r)

instance applicativeReader :: Applicative (Reader r) where
  pure a = Reader \ _ -> a

instance bindReader :: Bind (Reader r) where
  bind (Reader a) k = Reader \ r -> runReader (k (a r)) r

instance monadReader :: Monad (Reader r)

local :: forall r a. (r -> r) -> Reader r a -> Reader r a
local = withReader

ask :: forall r. Reader r r
ask = Reader id
