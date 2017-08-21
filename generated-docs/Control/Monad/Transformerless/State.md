## Module Control.Monad.Transformerless.State

#### `State`

``` purescript
newtype State s a
  = State (s -> Tuple a s)
```

##### Instances
``` purescript
Newtype (State s a) _
Functor (State s)
Apply (State s)
Applicative (State s)
Bind (State s)
Monad (State s)
Lazy (State s a)
MonadRec (State s)
```

#### `runState`

``` purescript
runState :: forall s a. State s a -> s -> Tuple a s
```

#### `evalState`

``` purescript
evalState :: forall s a. State s a -> s -> a
```

#### `execState`

``` purescript
execState :: forall s a. State s a -> s -> s
```

#### `mapState`

``` purescript
mapState :: forall s a b. (Tuple a s -> Tuple b s) -> State s a -> State s b
```

#### `mapS`

``` purescript
mapS :: forall s a b. (a -> b) -> State s a -> State s b
```

#### `(|->)`

``` purescript
infixl 4 mapS as |->
```

#### `applyS`

``` purescript
applyS :: forall s a b. State s (a -> b) -> State s a -> State s b
```

#### `(~)`

``` purescript
infixl 4 applyS as ~
```

#### `pureS`

``` purescript
pureS :: forall s a. a -> State s a
```

#### `bindS`

``` purescript
bindS :: forall s a b. State s a -> (a -> State s b) -> State s b
```

#### `(>>-)`

``` purescript
infixl 1 bindS as >>-
```

#### `deferS`

``` purescript
deferS :: forall s a. (Unit -> State s a) -> State s a
```

#### `tailRecS`

``` purescript
tailRecS :: forall s a b. (a -> State s (Step a b)) -> a -> State s b
```

#### `get`

``` purescript
get :: forall s. State s s
```

#### `gets`

``` purescript
gets :: forall s a. (s -> a) -> State s a
```

#### `put`

``` purescript
put :: forall s. s -> State s Unit
```

#### `modify`

``` purescript
modify :: forall s. (s -> s) -> State s Unit
```


