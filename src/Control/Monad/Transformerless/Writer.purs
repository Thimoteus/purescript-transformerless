module Control.Monad.Transformerless.Writer where

import Base

newtype Writer w a = Writer (Tuple a w)

runWriter :: forall w a. Writer w a -> Tuple a w
runWriter (Writer t) = t

execWriter :: forall w a. Writer w a -> w
execWriter (Writer (Tuple _ w)) = w

mapWriter :: forall w1 w2 a b. (Tuple a w1 -> Tuple b w2) -> Writer w1 a -> Writer w2 b
mapWriter f (Writer t) = Writer (f t)

instance functorWriter :: Functor (Writer w) where
  map f (Writer (Tuple a w)) = Writer (Tuple (f a) w)

instance applyWriter :: Semigroup w => Apply (Writer w) where
  apply (Writer (Tuple f w1)) (Writer (Tuple a w2)) = Writer (Tuple (f a) (w1 <> w2))

instance applicativeWriter :: Monoid w => Applicative (Writer w) where
  pure a = Writer (Tuple a mempty)

instance bindWriter :: Semigroup w => Bind (Writer w) where
  bind (Writer (Tuple a w)) k =
    let res = runWriter (k a)
        a' = fst res
        w' = snd res
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
