module Control.Monad.Transformerless.RWS
  ( RWS(..)
  , RWSResult(..)
  , runRWS
  , evalRWS
  , execRWS
  , mapRWS
  , withRWS
  , map_, (|->)
  , apply_, (~)
  , pure_
  , bind_, (>>-)
  , tailRec_
  -- Reader
  , reader
  , ask
  , local
  -- Writer
  , writer
  , listen
  , pass
  , tell
  , listens
  , censor
  -- State
  , state
  , get
  , gets
  , put
  , modify
  ) where

import Prelude

import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRec)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..), fst, snd, uncurry)

data RWSResult s a w = RWSResult s a w

rwstate :: forall s a w. RWSResult s a w -> s
rwstate (RWSResult s _ _) = s

resultws :: forall s a w. RWSResult s a w -> a
resultws (RWSResult _ a _) = a

rwriters :: forall s a w. RWSResult s a w -> w
rwriters (RWSResult _ _ w) = w

newtype RWS r w s a = RWS (r -> s -> RWSResult s a w)

derive instance newtypeRWS :: Newtype (RWS r w s a) _

runRWS :: forall r w s a. RWS r w s a -> r -> s -> RWSResult s a w
runRWS (RWS f) = f

evalRWS :: forall r w s a. RWS r w s a -> r -> s -> Tuple a w
evalRWS (RWS f) r s =
  let res = f r s
   in Tuple (resultws res) (rwriters res)

execRWS :: forall r w s a. RWS r w s a -> r -> s -> Tuple s w
execRWS (RWS f) r s =
  let res = f r s
   in Tuple (rwstate res) (rwriters res)

mapRWS :: forall r w1 w2 s a1 a2. (RWSResult s a1 w1 -> RWSResult s a2 w2) -> RWS r w1 s a1 -> RWS r w2 s a2
mapRWS f (RWS g) = RWS \ r s -> f (g r s)

withRWS :: forall r1 r2 w s a. (r2 -> s -> Tuple r1 s) -> RWS r1 w s a -> RWS r2 w s a
withRWS f (RWS g) = RWS \ r2 s -> uncurry g (f r2 s)

map_ :: forall r w s a b. (a -> b) -> RWS r w s a -> RWS r w s b
map_ f (RWS g) = RWS \ r s ->
  let res = g r s
   in RWSResult (rwstate res) (f (resultws res)) (rwriters res)

infixl 4 map_ as |->

apply_ :: forall r w s a b. Semigroup w => RWS r w s (a -> b) -> RWS r w s a -> RWS r w s b
apply_ (RWS ff) (RWS fa) = RWS \ r s ->
  let res = ff r s
      f = resultws res
      s' = rwstate res
      w' = rwriters res
      res' = fa r s'
      a = resultws res'
      b = f a
      s'' = rwstate res'
      w'' = w' <> rwriters res'
   in RWSResult s'' b w''

infixl 4 apply_ as ~

pure_ :: forall r w s a. Monoid w => a -> RWS r w s a
pure_ a = RWS \ r s -> RWSResult s a mempty

bind_ :: forall r w s a b. Semigroup w => RWS r w s a -> (a -> RWS r w s b) -> RWS r w s b
bind_ (RWS fa) k = RWS \ r s ->
  let res = fa r s
      a = resultws res
      s' = rwstate res
      w' = rwriters res
      res' = runRWS (k a) r s'
      s'' = rwstate res'
      w'' = w' <> rwriters res'
      b = resultws res'
   in RWSResult s'' b w''

infixl 1 bind_ as >>-

tailRec_ :: forall r w s a b. Monoid w => (a -> RWS r w s (Step a b)) -> a -> RWS r w s b
tailRec_ f a = RWS \ r s -> tailRec (k' r) (RWSResult s a mempty)
  where
  k' r (RWSResult st res wr) =
    let result = runRWS (f res) r st
        res' = resultws result
        st' = rwstate result
        wr' = rwriters result
     in case res' of
             Loop x -> Loop (RWSResult st' x (wr <> wr'))
             Done y -> Done (RWSResult st' y (wr <> wr'))

instance functorRWS :: Functor (RWS r w s) where
  map :: forall a b. (a -> b) -> RWS r w s a -> RWS r w s b
  map f (RWS g) = RWS \ r s ->
    let res = g r s
     in RWSResult (rwstate res) (f (resultws res)) (rwriters res)

instance applyRWS :: Semigroup w => Apply (RWS r w s) where
  apply :: forall a b. RWS r w s (a -> b) -> RWS r w s a -> RWS r w s b
  apply (RWS ff) (RWS fa) = RWS \ r s ->
    let res = ff r s
        f = resultws res
        s' = rwstate res
        w' = rwriters res
        res' = fa r s'
        a = resultws res'
        b = f a
        s'' = rwstate res'
        w'' = w' <> rwriters res'
     in RWSResult s'' b w''

instance applicativeRWS :: Monoid w => Applicative (RWS r w s) where
  pure :: forall a. a -> RWS r w s a
  pure a = RWS \ r s -> RWSResult s a mempty

instance bindRWS :: Semigroup w => Bind (RWS r w s) where
  bind :: forall a b. RWS r w s a -> (a -> RWS r w s b) -> RWS r w s b
  bind (RWS fa) k = RWS \ r s ->
    let res = fa r s
        a = resultws res
        s' = rwstate res
        w' = rwriters res
        res' = runRWS (k a) r s'
        s'' = rwstate res'
        w'' = w' <> rwriters res'
        b = resultws res'
     in RWSResult s'' b w''

instance monadRWS :: Monoid w => Monad (RWS r w s)

instance monadRecRWS :: Monoid w => MonadRec (RWS r w s) where
  tailRecM :: forall a b. (a -> RWS r w s (Step a b)) -> a -> RWS r w s b
  tailRecM f a = RWS \ r s -> tailRec (k' r) (RWSResult s a mempty)
    where
    k' r (RWSResult st res wr) =
      let result = runRWS (f res) r st
          res' = resultws result
          st' = rwstate result
          wr' = rwriters result
       in case res' of
               Loop x -> Loop (RWSResult st' x (wr <> wr'))
               Done y -> Done (RWSResult st' y (wr <> wr'))

-- | Reader

reader :: forall r w s a. Monoid w => (r -> a) -> RWS r w s a
reader f = RWS \ r s -> RWSResult s (f r) mempty

ask :: forall r w s. Monoid w => RWS r w s r
ask = RWS \ r s -> RWSResult s r mempty

local :: forall r w s a. (r -> r) -> RWS r w s a -> RWS r w s a
local f (RWS m) = RWS \ r s -> m (f r) s

-- | Writer

writer :: forall r w s a. Tuple a w -> RWS r w s a
writer (Tuple a w) = RWS \ r s -> RWSResult s a w

listen :: forall r w s a. RWS r w s a -> RWS r w s (Tuple a w)
listen (RWS f) = RWS \ r s ->
  let res = f r s
      s' = rwstate res
      a = resultws res
      w = rwriters res
   in RWSResult s' (Tuple a w) w

pass :: forall r w s a. RWS r w s (Tuple a (w -> w)) -> RWS r w s a
pass (RWS f) = RWS \ r s ->
  let res = f r s
      s' = rwstate res
      a = resultws res
      w = rwriters res
   in RWSResult s' (fst a) (snd a w)

tell :: forall r w s. w -> RWS r w s Unit
tell w = RWS \ r s -> RWSResult s unit w

listens :: forall r w s a b. (w -> b) -> RWS r w s a -> RWS r w s (Tuple a b)
listens f (RWS g) = RWS \ r s ->
  let res = g r s
      s' = rwstate res
      a = resultws res
      w = rwriters res
   in RWSResult s' (Tuple a (f w)) w

censor :: forall r w s a. Monoid w => (w -> w) -> RWS r w s a -> RWS r w s a
censor f m = pass do
  a <- m
  pure_ (Tuple a f)
    where
      bind = bind_

-- | State

state :: forall r w s a. Monoid w => (s -> Tuple a s) -> RWS r w s a
state f = RWS \ r s -> case f s of
  Tuple a s' -> RWSResult s' a mempty

get :: forall r w s. Monoid w => RWS r w s s
get = RWS \ r s -> RWSResult s s mempty

gets :: forall r w s a. Monoid w => (s -> a) -> RWS r w s a
gets f = f |-> get

put :: forall r w s. Monoid w => s -> RWS r w s Unit
put s = RWS \ r _ -> RWSResult s unit mempty

modify :: forall r w s. Monoid w => (s -> s) -> RWS r w s Unit
modify f = get >>- put <<< f
