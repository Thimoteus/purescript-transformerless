module Control.Monad.Transformerless.Cont
  ( Cont(..)
  , runCont
  , cont
  , callCC
  , mapCont
  , withCont
  ) where

import Prelude

import Data.Newtype (class Newtype)

newtype Cont r a = Cont ((a -> r) -> r)

derive instance newtypeCont :: Newtype (Cont r a) _

runCont :: forall r a. Cont r a -> ((a -> r) -> r)
runCont (Cont f) = f

cont :: forall a r. ((a -> r) -> r) -> Cont r a
cont = Cont

callCC :: forall a r. ((forall b. a -> Cont r b) -> Cont r a) -> Cont r a
callCC f = Cont \ ar -> (runCont (f \ a -> Cont \_ -> ar a)) ar

mapCont :: forall r a. (r -> r) -> Cont r a -> Cont r a
mapCont f (Cont k) = Cont \ ar -> f (k ar)

withCont :: forall a b r. ((b -> r) -> (a -> r)) -> Cont r a -> Cont r b
withCont k (Cont f) = Cont \ g -> f (k g)

instance functorCont :: Functor (Cont r) where
  map :: forall a b r. (a -> b) -> Cont r a -> Cont r b
  map f (Cont k) = Cont \ g -> k $ \ a -> g (f a)

instance applyCont :: Apply (Cont r) where
  apply :: forall a b r. Cont r (a -> b) -> Cont r a -> Cont r b
  apply (Cont ff) (Cont fa) = Cont \ fb -> ff (\ f -> fa (\ a -> fb (f a)))

instance applicativeCont :: Applicative (Cont r) where
  pure :: forall a r. a -> Cont r a
  pure x = Cont (_ $ x)

instance bindCont :: Bind (Cont r) where
  bind :: forall a b r. Cont r a -> (a -> Cont r b) -> Cont r b
  bind (Cont fa) k = Cont \ fb -> fa (\ a -> runCont (k a) fb)

instance monadCont :: Monad (Cont r)
