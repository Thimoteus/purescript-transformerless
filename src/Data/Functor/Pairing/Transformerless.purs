module Data.Functor.Pairing.Transformerless where

import Control.Comonad.Transformerless.Env (Env(..))
import Control.Comonad.Transformerless.Store (Store(..))
import Control.Comonad.Transformerless.Traced (Traced(..))
import Control.Monad.Transformerless.Reader (Reader)
import Control.Monad.Transformerless.State (State(..))
import Control.Monad.Transformerless.Writer (Writer(..))
import Data.Tuple (Tuple(..))

stateStore :: ∀ s a b c. (a -> b -> c) -> State s a -> Store s b -> c
stateStore f (State state) (Store get s) = f a (get s')
  where
  Tuple a s' = state s

readerEnv :: ∀ r a b c. (a -> b -> c) -> Reader r a -> Env r b -> c
readerEnv f reader (Env (Tuple e a)) = f (reader e) a

writerTraced :: ∀ w a b c. (a -> b -> c) -> Writer w a -> Traced w b -> c
writerTraced f (Writer writer) (Traced t) = (\(Tuple a w) f1 -> f a (f1 w)) writer t 