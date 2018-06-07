module Test.Main where

import Prelude

import Control.Monad.Rec.Class (Step(..))
import Control.Monad.Transformerless.Cont as Cont
import Control.Monad.Transformerless.RWS as RWS
import Control.Monad.Transformerless.Reader as Reader
import Control.Monad.Transformerless.State as State
import Control.Monad.Transformerless.Writer as Writer
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log, logShow)

foreign import t :: Effect Number

loop :: Int -> RWS.RWS String (Array String) Int Unit
loop n = RWS.tailRec_ go n where
  go 0 = do
    RWS.tell ["Done!"]
    RWS.pure_ (Done unit)
      where
        bind = RWS.bind_
        discard = RWS.bind_
  go m = do
    x <- RWS.get
    RWS.put (x + 1)
    RWS.pure_ (Loop (m - 1))
      where
        bind = RWS.bind_
        discard = RWS.bind_

loopState :: Int -> State.State Int Unit
loopState n = State.tailRecS go n where
  go 0 = State.pureS (Done unit)
  go m = do
    x <- State.get
    State.put (x + 1)
    State.pureS (Loop (m - 1))
      where
        bind = State.bindS
        discard = State.bindS

testRWS :: Effect Unit
testRWS = do
  t1 <- t
  let res1 = RWS.runRWS (loop 1000000) "" 0
  t2 <- t
  log $ "RWS: " <> show (t2 - t1)
  t3 <- t
  let res2 = State.execState (loopState 1000000) 0
  t4 <- t
  log $ "State: " <> show (t4 - t3)

readerTest :: Reader.Reader String String
readerTest = Reader.local (_ <> "!") Reader.ask

testReader :: Effect Unit
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

testState :: Effect Unit
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

testWriter :: Effect Unit
testWriter = case Writer.runWriter writerTest of
  Tuple value output -> do
    log output
    logShow value

contTest :: forall r. Cont.Cont r Int
contTest = Cont.callCC \ return -> do
  let n = 5
  return n :: Cont.Cont r Unit
  pure 15

testCont :: Effect Unit
testCont = Cont.runCont contTest logShow

main :: Effect Unit
main = do
  testReader
  testState
  testWriter
  testRWS
  testCont
