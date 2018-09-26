module Control.Monad.Transformerless.Cont
  ( Cont(..)
  , runCont
  , cont
  , callCC
  , mapCont
  , withCont
  ) where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Plus (class Plus)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Ref as Ref

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

-- | Parallel Cont

newtype ParCont a = ParCont (Cont (Effect Unit) a)

derive instance newtypeParCont :: Newtype (ParCont a) _

derive instance functorParCont :: Functor ParCont

instance applyParCont :: Apply ParCont where
  apply (ParCont cab) (ParCont ca) = ParCont $ Cont \k -> do
    rab <- Ref.new Nothing
    ra <- Ref.new Nothing

    runCont cab \ab -> do
      ma <- Ref.read ra
      case ma of
        Just a -> k (ab a)
        _ -> Ref.write (Just ab) rab
    
    runCont ca \a -> do
      mab <- Ref.read rab
      case mab of
        Just ab -> k (ab a)
        _ -> Ref.write (Just a) ra

instance applicativeParCont :: Applicative ParCont where
  pure = ParCont <<< pure

instance altParCont :: Alt ParCont where
  alt (ParCont ca) (ParCont cb) = ParCont $ Cont \k -> do
    doneRef <- Ref.new false

    runCont ca \a -> do
      done <- Ref.read doneRef
      unless done do
        Ref.write true doneRef
        k a
    
    runCont cb \b -> do
      done <- Ref.read doneRef
      unless done do
        Ref.write true doneRef
        k b

instance plusParCont :: Plus ParCont where
  empty = ParCont (Cont mempty)

instance alternativeParCont :: Alternative ParCont

sequential :: ParCont ~> Cont (Effect Unit)
sequential (ParCont c) = c

parallel :: Cont (Effect Unit) ~> ParCont
parallel = ParCont