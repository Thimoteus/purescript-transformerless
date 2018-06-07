module Control.Monad.Transformerless.Reader where

import Prelude

type Reader r = Function r

runReader :: forall r a. Reader r a -> r -> a
runReader = identity

withReader :: forall r1 r2 a. (r2 -> r1) -> Reader r1 a -> Reader r2 a
withReader = (>>>)

mapReader :: forall r a b. (a -> b) -> Reader r a -> Reader r b
mapReader = (<<<)

infixl 4 mapReader as |->

applyR :: forall r a b. Reader r (a -> b) -> Reader r a -> Reader r b
applyR f a = \ r -> f r (a r)

infixl 4 applyR as ~

pureR :: forall r a. a -> Reader r a
pureR a = \ _ -> a

bindR :: forall r a b. Reader r a -> (a -> Reader r b) -> Reader r b
bindR a k = \ r -> k (a r) r

infixl 1 bindR as >>-

local :: forall r a. (r -> r) -> Reader r a -> Reader r a
local = (>>>)

ask :: forall r. Reader r r
ask = identity
