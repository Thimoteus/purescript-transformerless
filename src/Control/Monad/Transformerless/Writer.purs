module Control.Monad.Transformerless.Writer where

import Prelude

import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(Tuple))

newtype Writer w a = Writer (Tuple a w)

derive instance newtypeWriter :: Newtype (Writer w a) _

runWriter :: forall w a. Writer w a -> Tuple a w
runWriter (Writer t) = t

execWriter :: forall w a. Writer w a -> w
execWriter (Writer (Tuple _ w)) = w

mapWriter :: forall w1 w2 a b. (Tuple a w1 -> Tuple b w2) -> Writer w1 a -> Writer w2 b
mapWriter f (Writer t) = Writer (f t)

mapW :: forall w a b. (a -> b) -> Writer w a -> Writer w b
mapW f (Writer (Tuple a w)) = Writer (Tuple (f a) w)

infixl 4 mapW as |->

applyW :: forall w a b. Semigroup w => Writer w (a -> b) -> Writer w a -> Writer w b
applyW (Writer (Tuple f w1)) (Writer (Tuple a w2)) = Writer (Tuple (f a) (w1 <> w2))

infixl 4 applyW as ~

pureW :: forall w a. Monoid w => a -> Writer w a
pureW a = Writer (Tuple a mempty)

bindW :: forall w a b. Semigroup w => Writer w a -> (a -> Writer w b) -> Writer w b
bindW (Writer (Tuple a w)) k =
  let Tuple a' w' = runWriter (k a)
   in Writer (Tuple a' (w <> w'))

infixl 1 bindW as >>-

instance functorWriter :: Functor (Writer w) where
  map :: forall a b. (a -> b) -> Writer w a -> Writer w b
  map f (Writer (Tuple a w)) = Writer (Tuple (f a) w)

instance applyWriter :: Semigroup w => Apply (Writer w) where
  apply :: forall a b. Writer w (a -> b) -> Writer w a -> Writer w b
  apply (Writer (Tuple f w1)) (Writer (Tuple a w2)) = Writer (Tuple (f a) (w1 <> w2))

instance applicativeWriter :: Monoid w => Applicative (Writer w) where
  pure :: forall a. a -> Writer w a
  pure a = Writer (Tuple a mempty)

instance bindWriter :: Semigroup w => Bind (Writer w) where
  bind :: forall a b. Writer w a -> (a -> Writer w b) -> Writer w b
  bind (Writer (Tuple a w)) k =
    let Tuple a' w' = runWriter (k a)
     in Writer (Tuple a' (w <> w'))

instance monadWriter :: Monoid w => Monad (Writer w)

pass :: forall w a. Writer w (Tuple a (w -> w)) -> Writer w a
pass (Writer (Tuple (Tuple a f) w)) = Writer (Tuple a (f w))

listen :: forall w a. Writer w a -> Writer w (Tuple a w)
listen (Writer (Tuple a w)) = Writer (Tuple (Tuple a w) w)

tell :: forall w. w -> Writer w Unit
tell w = Writer (Tuple unit w)

listens :: forall w a b. Monoid w => (w -> b) -> Writer w a -> Writer w (Tuple a b)
listens f (Writer (Tuple a w)) = Writer (Tuple (Tuple a (f w)) w)

censor :: forall w a. Monoid w => (w -> w) -> Writer w a -> Writer w a
censor f m = pass do
  a <- m
  pure (Tuple a f)
    where
      bind = bindW
