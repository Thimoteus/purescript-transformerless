module Control.Monad.Transformerless.Except where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Extend (class Extend)
import Control.MonadPlus (class MonadPlus, class MonadZero)
import Control.Plus (class Plus)
import Data.Bifoldable (class Bifoldable)
import Data.Bifunctor (class Bifunctor, lmap)
import Data.Bitraversable (class Bitraversable)
import Data.Either (Either(..), either)
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable)
import Data.Functor.Invariant (class Invariant)
import Data.Newtype (class Newtype, over)
import Data.Ord (class Ord1)
import Data.Traversable (class Traversable)

newtype Except e a = Except (Either e a)

derive instance newtypeExcept :: Newtype (Except e a) _

derive newtype instance invariantExcept :: Invariant (Except e)
derive newtype instance bifunctorExcept :: Bifunctor Except
derive newtype instance functorExcept :: Functor (Except e)
derive newtype instance applyExcept :: Apply (Except e)
derive newtype instance applicativeExcept :: Applicative (Except e)
derive newtype instance bindExcept :: Bind (Except e)
derive newtype instance monadExcept :: Monad (Except e)
derive newtype instance extendExcept :: Extend (Except e)
derive newtype instance eqExcept :: (Eq e, Eq a) => Eq (Except e a)
derive newtype instance eq1Except :: Eq e => Eq1 (Except e)
derive newtype instance ordExcept :: (Ord e, Ord a) => Ord (Except e a)
derive newtype instance ord1Except :: Ord e => Ord1 (Except e)
derive newtype instance boundedExcept :: (Bounded e, Bounded a) => Bounded (Except e a)
derive newtype instance foldableExcept :: Foldable (Except e)
derive newtype instance bifoldableExcept :: Bifoldable Except
derive newtype instance traversableExcept :: Traversable (Except e)
derive newtype instance bitraversableExcept :: Bitraversable Except
-- derive newtype instance semiringExcept :: Semiring a => Semiring (Except e a)
derive newtype instance semigroupExcept :: Semigroup a => Semigroup (Except e a)

instance alternativeExcept :: Monoid e => Alternative (Except e)
instance monadZeroExcept :: Monoid e => MonadZero (Except e)
instance monadPlusExcept :: Monoid e => MonadPlus (Except e)

instance showExcept :: (Show e, Show a) => Show (Except e a) where
  show (Except a) = "(Except " <> show a <> ")"

-- | The `Alt` instance differs from that belonging to the underlying `Either`
-- | in that this version collects errors.
instance altExcept :: Semigroup e => Alt (Except e) where
  alt x@(Except (Right _)) _ = x
  alt _ y@(Except (Right _)) = y
  alt (Except (Left e1)) (Except (Left e2)) = Except (Left (e1 <> e2))

instance plusExcept :: Monoid e => Plus (Except e) where
  empty = Except (Left mempty)

runExcept :: forall e. Except e ~> Either e
runExcept (Except ex) = ex

withExcept :: forall e1 e2. (e1 -> e2) -> Except e1 ~> Except e2
withExcept = lmap

mapExcept :: forall e1 e2 a1 a2. (Either e1 a1 -> Either e2 a2) -> Except e1 a1 -> Except e2 a2
mapExcept = over Except

throwError :: forall e a. e -> Except e a
throwError e = Except (Left e)

catchError :: forall e a. Except e a -> (e -> Except e a) -> Except e a
catchError e f = either f pure (runExcept e)

except :: forall e a. Either e a -> Except e a
except = Except