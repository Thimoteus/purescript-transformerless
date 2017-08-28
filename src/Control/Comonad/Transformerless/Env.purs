module Control.Comonad.Transformerless.Env
  ( Env(..)
  , runEnv
  , withEnv
  , mapEnv
  , env
  , ask
  , asks
  , local
  ) where

import Prelude

import Control.Comonad (class Comonad, class Extend)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))

newtype Env e a = Env (Tuple e a)

derive instance newtypeEnv :: Newtype (Env e a) _

runEnv :: forall e a. Env e a -> Tuple e a
runEnv (Env t) = t

withEnv :: forall e1 e2 a. (e1 -> e2) -> Env e1 a -> Env e2 a
withEnv f (Env (Tuple e a)) = Env (Tuple (f e) a)

mapEnv :: forall a b e. (a -> b) -> Env e a -> Env e b
mapEnv f (Env (Tuple e a)) = Env (Tuple e (f a))

env :: forall a e. e -> a -> Env e a
env e a = Env (Tuple e a)

ask :: forall a e. Env e a -> e
ask (Env (Tuple e _)) = e

asks :: forall a e1 e2. (e1 -> e2) -> Env e1 a -> e2
asks f (Env (Tuple e a)) = f e

local :: forall e a. (e -> e) -> Env e a -> Env e a
local f (Env (Tuple e a)) = Env (Tuple (f e) a)

instance functorEnv :: Functor (Env e) where
  map :: forall a b. (a -> b) -> Env e a -> Env e b
  map f (Env (Tuple e a)) = Env (Tuple e (f a))

-- | Law: extend f <<< extend g = extend (f <<< extend g)
-- | Proof:
-- | First, rewrite in pointful form:
-- | (extend f <<< extend g) x = extend (f <<< extend g) x
-- | RHS := extend (f <<< extend g) x =
-- | extend (\ y -> f (extend g y)) x =
-- | extend (\ (y1, y2) -> f (extend g (y1, y2))) (x1, x2) =
-- | extend (\ (y1, y2) -> f (y1, g (y1, y2))) (x1, x2) =
-- | (x1, (\ (y1, y2) -> f (y1, g (y1, y2))) (x1, x2)) =
-- | (x1, f (x1, g (x1, x2)))
-- | LHS := (\ y -> extend f (extend g y)) x =
-- | (\ (y1, y2) -> extend f (extend g (y1, y2))) (x1, x2) =
-- | (\ (y1, y2) -> extend f (y1, g (y1, y2))) (x1, x2) =
-- | (\ (y1, y2) -> (y1, f (y1, g (y1, y2)))) (x1, x2) =
-- | (x1, f (x1, g (x1, x2)))
instance extendEnv :: Extend (Env e) where
  extend :: forall b a. (Env e a -> b) -> Env e a -> Env e b
  extend k en@(Env (Tuple e _)) = Env (Tuple e (k en))

-- | Law: extract <<= xs = xs
-- | Proof:
-- | LHS := extend extract xs =
-- | extend extract (x1, x2) =
-- | (x1, extract (x1, x2)) =
-- | (x1, x2)
-- | RHS := (x1, x2)
-- | Law: extract (f <<= xs) = f xs
-- | Proof:
-- | LHS := extract (extend f xs) =
-- | extract (extend f (x1, x2)) =
-- | extract (x1, f (x1, x2)) =
-- | f (x1, x2)
-- | RHS := f (x1, x2)
instance comonadEnv :: Comonad (Env e) where
  extract :: forall a. Env e a -> a
  extract (Env (Tuple _ a)) = a
