module Control.Comonad.Transformerless.Traced
  ( Traced(..)
  , runTraced
  , traced
  , track
  , tracks
  , listen
  , listens
  , censor
  ) where

import Prelude

import Control.Comonad (class Comonad, class Extend)
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))

newtype Traced m a = Traced (m -> a)

derive instance newtypeTraced :: Newtype (Traced w a) _

runTraced :: forall m a. Traced m a -> m -> a
runTraced (Traced f) = f

traced :: forall m a. (m -> a) -> Traced m a
traced = Traced

-- | Law: track mempty = extract
-- | Proof:
-- | First, rewrite as: track mempty f = extract f
-- | RHS := f mempty
-- | LHS := f mempty
-- | Law: (track s =<= track t) x = track (s <> t) x
-- | Proof:
-- | RHS := track (s <> t) x = x (s <> t)
-- | LHS := composeCoKliesliFlipped (track s) (track t) x =
-- | track s (track t <<= x) =
-- | track s (extend (track t) x) =
-- | track s (\ t' -> (track t) \ t'' -> x (t' <> t'')) =
-- | track s (\ t' -> x (t' <> t)) =
-- | x (s <> t)
track :: forall a m. Monoid m => m -> Traced m a -> a
track m (Traced f) = f m

tracks :: forall a m. Monoid m => (a -> m) -> Traced m a -> a
tracks f w@(Traced g) = track (f (g mempty)) w

listen :: forall a m. Traced m a -> Traced m (Tuple a m)
listen (Traced ma) = Traced \ m -> Tuple (ma m) m

listens :: forall a b m. (m -> b) -> Traced m a -> Traced m (Tuple a b)
listens mb (Traced ma) = Traced \ m -> Tuple (ma m) (mb m)

censor :: forall a m. (m -> m) -> Traced m a -> Traced m a
censor mm (Traced ma) = Traced \ m -> ma (mm m)

instance functorTraced :: Functor (Traced m) where
  map :: forall a b m. (a -> b) -> Traced m a -> Traced m b
  map f (Traced g) = Traced \ x -> f (g x)

-- | Law: extend f <<< extend g = extend (f <<< extend g)
-- | Proof:
-- | Rewrite pointfully:
-- | (extend f <<< extend g) x = extend (f <<< extend g) x
-- | LHS := extend f (extend g x) =
-- | extend f (\ t -> g \ t' -> x (t <> t')) =
-- | \ s -> f (\ s' -> (\ t -> g \ t' -> x (t <> t')) (s <> s')) =
-- | \ s -> f (\ s' -> g \ t' -> x (s <> s' <> t'))
-- | RHS := \ s -> (f <<< extend g) (\ s' -> x (s <> s')) =
-- | \ s -> (\ y -> f (extend g y)) (\ s' -> x (s <> s')) =
-- | \ s -> (\ y -> f (\ t -> g (\ t' -> y (t <> t')))) (\ s' -> x (s <> s')) =
-- | \ s -> f (\ t -> g (\ t' -> (\ s' -> x (s <> s')) (t <> t'))) =
-- | \ s -> f (\ t -> g (\ t' -> x (s <> t <> t'))) = (via renaming)
-- | \ s -> f (\ s' -> g \ t' -> x (s <> s' <> t'))
instance extendTraced :: Semigroup m => Extend (Traced m) where
  extend :: forall b a. (Traced m a -> b) -> Traced m a -> Traced m b
  extend f (Traced g) = Traced \ t -> f (Traced \ t' -> g (t <> t'))

-- | Law: extract <<= xs = xs
-- | Proof:
-- | RHS := \ x -> s
-- | LHS := extend extract (\ x -> s) =
-- | \ t -> extract (\ t' -> (\ x -> s) (t <> t')) =
-- | \ t -> (\ x -> s) (t <> mempty) =
-- | \ t -> (\ x -> s) t =
-- | \ x -> s
instance comonadTraced :: Monoid m => Comonad (Traced m) where
  extract :: forall a. Traced m a -> a
  extract (Traced f) = f mempty
