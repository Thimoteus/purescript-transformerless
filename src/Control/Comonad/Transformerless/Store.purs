module Control.Comonad.Transformerless.Store
  ( Store(..)
  , runStore
  , store
  , peek
  , pos
  ) where

import Prelude

import Control.Comonad (class Comonad, class Extend)
import Data.Tuple (Tuple(..))

data Store s a = Store (s -> a) s

runStore :: forall s a. Store s a -> Tuple (s -> a) s
runStore (Store f s) = Tuple f s

store :: forall s a. (s -> a) -> s -> Store s a
store = Store

-- | 1. Law: peek (pos x) x = extract x
-- | 2. Proof:
-- | 3. RHS := extract (f, s) = f s
-- | 4. LHS := peek (pos (f, s)) (f, s) =
-- | 5. peek s (f, s) =
-- | 6. peek s (f, _) = f s
peek :: forall s a. s -> Store s a -> a
peek s (Store sa _) = sa s

-- | 1. Law: pos (extend _ x) = pos x
-- | 2. Proof:
-- | 3. RHS := pos (_, s)
-- | 4. LHS := pos (extend _ (f, s)) =
-- | 5. pos (extend _ (_, s)) =
-- | 6. pos (_, s)
pos :: forall s a. Store s a -> s
pos (Store _ s) = s

-- | 1. Law: experiment f x = flip peek x <$> f (pos x)
-- | 2. Proof:
-- | 3. LHS := experiment f (sa, s) = sa <$> f s
-- | 4. RHS := flip peek (sa, s) <$> f (pos (sa, s)) =
-- | 5. flip peek (sa, s) <$> f s =
-- | 6. (\ x -> peek x (sa, s)) <$> f s =
-- | 7. (\ x -> sa x) <$> f s =
-- | 8. sa <$> f s
experiment :: forall f s a. Functor f => (s -> f s) -> Store s a -> f a
experiment sfs (Store sa s) = sa <$> sfs s

-- | 1. Law: peeks f x = peek (f $ pos x) x
-- | 2. Proof:
-- | 3. LHS := peeks f (sa, s) = sa (f s)
-- | 4. RHS := peek (f (pos (sa, s))) (sa, s) =
-- | 5. peek (f s) (sa, s) =
-- | 6. sa (f s)
peeks :: forall s a. (s -> s) -> Store s a -> a
peeks ss (Store sa s) = sa (ss s)

-- TODO: Optimize
seek :: forall s a. s -> Store s a -> Store s a
seek s = peek s <<< duplicate

-- TODO: Optimize
seeks :: forall s a. (s -> s) -> Store s a -> Store s a
seeks ss = peeks ss <<< duplicate

duplicate :: ∀ s a. Store s a -> Store s (Store s a)
duplicate (Store f s) = Store (Store f) s

instance functorStore :: Functor (Store s) where
  map :: forall a b. (a -> b) -> Store s a -> Store s b
  map f (Store g s) = Store (f <<< g) s

instance extendStore :: Extend (Store s) where
  extend :: forall a b s. (Store s a -> b) -> Store s a -> Store s b
  extend f = map f <<< duplicate

instance comonadStore :: Comonad (Store s) where
  extract :: forall a. Store s a -> a
  extract (Store f s) = f s
