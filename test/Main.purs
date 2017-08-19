module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Rec.Class (Step(..))
import Control.Monad.Transformerless.RWS as RWS
import Control.Monad.Transformerless.Reader as Reader
import Control.Monad.Transformerless.State as State
import Control.Monad.Transformerless.Writer as Writer
import Data.Tuple (Tuple(..))

foreign import t :: forall eff. Eff eff Number

loop :: Int -> RWS.RWS String (Array String) Int Unit
loop n = RWS.tailRec_ go n where
  go 0 = do
    RWS.tell ["Done!"]
    RWS.pure_ (Done unit)
      where
        bind = RWS.bind_
  go m = do
    x <- RWS.get
    RWS.put (x + 1)
    RWS.pure_ (Loop (m - 1))
      where
        bind = RWS.bind_

loopState :: Int -> State.State Int Unit
loopState n = State.tailRecS go n where
  go 0 = State.pureS (Done unit)
  go m = do
    x <- State.get
    State.put (x + 1)
    State.pureS (Loop (m - 1))
      where
        bind = State.bindS

testRWS :: forall e. Eff (console :: CONSOLE | e) Unit
testRWS = do
  t1 <- t
  let res1 = RWS.runRWS (loop 10000000) "" 0
  t2 <- t
  log $ "RWS: " <> show (t2 - t1)
  t3 <- t
  let res2 = State.execState (loopState 10000000) 0
  t4 <- t
  log $ "State: " <> show (t4 - t3)

readerTest :: Reader.Reader String String
readerTest = Reader.local (_ <> "!") Reader.ask

testReader :: forall e. Eff (console :: CONSOLE | e) Unit
testReader = log $ Reader.runReader readerTest "Done"

incState :: State.State Int Unit
incState = State.modify (add 1)

stateTest :: State.State Int String
stateTest = do
  incState
  incState
  incState
  incState
  incState
  incState
  pure "Done"
    where
      bind = State.bindS

testState :: forall e. Eff (console :: CONSOLE | e) Unit
testState = case State.runState stateTest 0 of
  Tuple value state -> do
    log $ "state: " <> show state
    log $ "value: " <> show value

writerTest :: Writer.Writer String Int
writerTest = do
  Writer.tell "Hello from writerTest"
  pure 42
    where
      bind = Writer.bindW

testWriter :: forall e. Eff (console :: CONSOLE | e) Unit
testWriter = case Writer.runWriter writerTest of
  Tuple value output -> do
    log output
    logShow value

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  testReader
  testState
  testWriter
  testRWS
