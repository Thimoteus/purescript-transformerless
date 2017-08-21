module Control.Comonad.Transformerless.Store
  ( Store(..)
  , runStore
  , store
  , peek
  , pos
  ) where

import Prelude

import Control.Comonad (class Comonad, class Extend, duplicate)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))

newtype Store s a = Store (Tuple (s -> a) s)

derive instance newtypeStore :: Newtype (Store s a) _

runStore :: forall s a. Store s a -> Tuple (s -> a) s
runStore (Store s) = s

store :: forall s a. Tuple (s -> a) s -> Store s a
store = Store

-- | Law: peek (pos x) x = extract x
-- | Proof:
-- | RHS := extract (f, s) = f s
-- | LHS := peek (pos (f, s)) (f, s) = 
-- | peek s (f, s) =
-- | peek s (f, _) = f s
peek :: forall s a. s -> Store s a -> a
peek s (Store (Tuple sa _)) = sa s

-- | Law: pos (extend _ x) = pos x
-- | Proof:
-- | RHS := pos (_, s)
-- | LHS := pos (extend _ (f, s)) =
-- | pos (extend _ (_, s)) =
-- | pos (_, s)
pos :: forall s a. Store s a -> s
pos (Store (Tuple _ s)) = s

-- | Law: experiment f x = flip peek x <$> f (pos x)
-- | Proof:
-- | LHS := experiment f (sa, s) = sa <$> f s
-- | RHS := flip peek (sa, s) <$> f (pos (sa, s)) =
-- | flip peek (sa, s) <$> f s =
-- | (\ x -> peek x (sa, s)) <$> f s =
-- | (\ x -> sa x) <$> f s =
-- | sa <$> f s
experiment :: forall f s a. Functor f => (s -> f s) -> Store s a -> f a
experiment sfs (Store (Tuple sa s)) = sa <$> sfs s

-- | Law: peeks f x = peek (f $ pos x) x
-- | Proof:
-- | LHS := peeks f (sa, s) = sa (f s)
-- | RHS := peek (f (pos (sa, s))) (sa, s) =
-- | peek (f s) (sa, s) =
-- | sa (f s)
peeks :: forall s a. (s -> s) -> Store s a -> a
peeks ss (Store (Tuple sa s)) = sa (ss s)

-- TODO: Optimize
seek :: forall s a. s -> Store s a -> Store s a
seek s = peek s <<< duplicate

-- TODO: Optimize
seeks :: forall s a. (s -> s) -> Store s a -> Store s a
seeks ss = peeks ss <<< duplicate

instance functorStore :: Functor (Store s) where
  map :: forall a b. (a -> b) -> Store s a -> Store s b
  map f (Store (Tuple g s)) = Store (Tuple (f <<< g) s)

instance extendStore :: Extend (Store s) where
  extend :: forall a b s. (Store s a -> b) -> Store s a -> Store s b
  extend f st@(Store (Tuple _ s)) = Store (Tuple (\ s' -> f st) s)

instance comonadStore :: Comonad (Store s) where
  extract :: forall a. Store s a -> a
  extract (Store (Tuple f s)) = f s
