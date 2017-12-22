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

-- | 1. Law: track mempty = extract
-- | 2. Proof:
-- | 3. First, rewrite as: track mempty f = extract f
-- | 4. RHS := f mempty
-- | 5. LHS := f mempty
-- | 6. Law: (track s =<= track t) x = track (s <> t) x
-- | 7. Proof:
-- | 8. RHS := track (s <> t) x = x (s <> t)
-- | 9. LHS := composeCoKliesliFlipped (track s) (track t) x =
-- | 10. track s (track t <<= x) =
-- | 11. track s (extend (track t) x) =
-- | 12. track s (\ t' -> (track t) \ t'' -> x (t' <> t'')) =
-- | 13. track s (\ t' -> x (t' <> t)) =
-- | 14. x (s <> t)
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

-- | 1. Law: extend f <<< extend g = extend (f <<< extend g)
-- | 2. Proof:
-- | 3. Rewrite pointfully:
-- | 4. (extend f <<< extend g) x = extend (f <<< extend g) x
-- | 5. LHS := extend f (extend g x) =
-- | 6. extend f (\ t -> g \ t' -> x (t <> t')) =
-- | 7. \ s -> f (\ s' -> (\ t -> g \ t' -> x (t <> t')) (s <> s')) =
-- | 8. \ s -> f (\ s' -> g \ t' -> x (s <> s' <> t'))
-- | 9. RHS := \ s -> (f <<< extend g) (\ s' -> x (s <> s')) =
-- | 10. \ s -> (\ y -> f (extend g y)) (\ s' -> x (s <> s')) =
-- | 11. \ s -> (\ y -> f (\ t -> g (\ t' -> y (t <> t')))) (\ s' -> x (s <> s')) =
-- | 12. \ s -> f (\ t -> g (\ t' -> (\ s' -> x (s <> s')) (t <> t'))) =
-- | 13. \ s -> f (\ t -> g (\ t' -> x (s <> t <> t'))) = (via renaming)
-- | 14. \ s -> f (\ s' -> g \ t' -> x (s <> s' <> t'))
instance extendTraced :: Semigroup m => Extend (Traced m) where
  extend :: forall b a. (Traced m a -> b) -> Traced m a -> Traced m b
  extend f (Traced g) = Traced \ t -> f (Traced \ t' -> g (t <> t'))

-- | 1. Law: extract <<= xs = xs
-- | 2. Proof:
-- | 3. RHS := \ x -> s
-- | 4. LHS := extend extract (\ x -> s) =
-- | 5. \ t -> extract (\ t' -> (\ x -> s) (t <> t')) =
-- | 6. \ t -> (\ x -> s) (t <> mempty) =
-- | 7. \ t -> (\ x -> s) t =
-- | 8. \ x -> s
instance comonadTraced :: Monoid m => Comonad (Traced m) where
  extract :: forall a. Traced m a -> a
  extract (Traced f) = f mempty
