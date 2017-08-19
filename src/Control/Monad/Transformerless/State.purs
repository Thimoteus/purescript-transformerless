module Control.Monad.Transformerless.State where

import Prelude

import Control.Lazy (class Lazy)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRec)
import Data.Tuple (Tuple(..), fst, snd)

newtype State s a = State (s -> Tuple a s)

runState :: forall s a. State s a -> s -> Tuple a s
runState (State s) = s

evalState :: forall s a. State s a -> s -> a
evalState (State s) i = fst (s i)

execState :: forall s a. State s a -> s -> s
execState (State s) i = snd (s i)

mapState :: forall s a b. (Tuple a s -> Tuple b s) -> State s a -> State s b
mapState f (State s) = State (f <<< s)

mapS :: forall s a b. (a -> b) -> State s a -> State s b
mapS f (State s) = State \ st ->
  let Tuple a s' = s st
   in Tuple (f a) s'

infixl 4 mapS as |->

applyS :: forall s a b. State s (a -> b) -> State s a -> State s b
applyS (State ff) (State fa) = State \ s ->
  let Tuple f s' = ff s
      Tuple a s'' = fa s'
   in Tuple (f a) s''

infixl 4 applyS as ~

pureS :: forall s a. a -> State s a
pureS a = State (Tuple a)

bindS :: forall s a b. State s a -> (a -> State s b) -> State s b
bindS (State fa) k = State \ s ->
  let Tuple a s' = fa s
      Tuple b s'' = runState (k a) s'
   in Tuple b s''

infixl 1 bindS as >>-

deferS :: forall s a. (Unit -> State s a) -> State s a
deferS f = State (runState (f unit))

tailRecS :: forall s a b. (a -> State s (Step a b)) -> a -> State s b
tailRecS f a = State \ s -> tailRec f' (Tuple a s)
  where
  f' (Tuple x s) =
    let Tuple m s1 = runState (f x) s
     in case m of
         Loop l -> Loop (Tuple l s1)
         Done r -> Done (Tuple r s1)

instance functorState :: Functor (State s) where
  map f (State s) = State \ st ->
    let Tuple a s' = s st
     in Tuple (f a) s'

instance applyState :: Apply (State s) where
  apply (State ff) (State fa) = State \ s ->
    let Tuple f s' = ff s
        Tuple a s'' = fa s'
     in Tuple (f a) s''

instance applicativeState :: Applicative (State s) where
  pure a = State (Tuple a)

instance bindState :: Bind (State s) where
  bind (State fa) k = State \ s ->
    let Tuple a s' = fa s
        Tuple b s'' = runState (k a) s'
     in Tuple b s''

instance monadState :: Monad (State s)

instance lazyState :: Lazy (State s a) where
  defer f = State (runState (f unit))

instance monadrecState :: MonadRec (State s) where
  tailRecM f a = State \ s -> tailRec f' (Tuple a s)
    where
    f' (Tuple x s) =
      let Tuple m s1 = runState (f x) s
       in case m of
           Loop l -> Loop (Tuple l s1)
           Done r -> Done (Tuple r s1)

get :: forall s. State s s
get = State \ st -> Tuple st st

gets :: forall s a. (s -> a) -> State s a
gets f = State \ st -> Tuple (f st) st

put :: forall s. s -> State s Unit
put s = State \ _ -> Tuple unit s

modify :: forall s. (s -> s) -> State s Unit
modify f = State \ s -> Tuple unit (f s)
