module Test.Main where

import Base
import Control.Monad.Transformerless.RWS as RWS
import Control.Monad.Transformerless.State as State
import Control.Monad.Transformerless.Reader as Reader
import Control.Monad.Transformerless.Writer as Writer
import Control.Monad.Rec.Class (tailRecM)

foreign import t :: forall eff. Eff eff Number

loop :: Int -> RWS.RWS String (Array String) Int Unit
loop n = tailRecM go n where
  go 0 = do
    RWS.tell ["Done!"]
    pure (Right unit)
  go n = do
    x <- RWS.get
    RWS.put (x + 1)
    pure (Left (n - 1))

loopState :: Int -> State.State Int Unit
loopState n = tailRecM go n where
  go 0 = do
    pure (Right unit)
  go n = do
    x <- State.get
    State.put (x + 1)
    pure (Left (n - 1))

testRWS :: forall e. Eff (console :: CONSOLE | e) Unit
testRWS = do
  t1 <- t
  res1 <- pure $ RWS.runRWS (loop 10000000) "" 0
  t2 <- t
  log $ "RWS: " <> show (t2 - t1)
  t3 <- t
  res2 <- pure $ State.execState (loopState 10000000) 0
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

testState :: forall e. Eff (console :: CONSOLE | e) Unit
testState = case State.runState stateTest 0 of
  Tuple value state -> do
    log $ "state: " <> show state
    log $ "value: " <> show value

writerTest :: Writer.Writer String Int
writerTest = do
  Writer.tell "Hello from writerTest"
  pure 42

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
