## Module Control.Monad.Transformerless.RWS

#### `RWSResult`

``` purescript
data RWSResult s a w
  = RWSResult s a w
```

#### `RWS`

``` purescript
newtype RWS r w s a
  = RWS (r -> s -> RWSResult s a w)
```

##### Instances
``` purescript
Functor (RWS r w s)
(Semigroup w) => Apply (RWS r w s)
(Monoid w) => Applicative (RWS r w s)
(Semigroup w) => Bind (RWS r w s)
(Monoid w) => Monad (RWS r w s)
(Monoid w) => MonadRec (RWS r w s)
```

#### `runRWS`

``` purescript
runRWS :: forall r w s a. RWS r w s a -> r -> s -> RWSResult s a w
```

#### `evalRWS`

``` purescript
evalRWS :: forall r w s a. RWS r w s a -> r -> s -> Tuple a w
```

#### `execRWS`

``` purescript
execRWS :: forall r w s a. RWS r w s a -> r -> s -> Tuple s w
```

#### `mapRWS`

``` purescript
mapRWS :: forall r w1 w2 s a1 a2. (RWSResult s a1 w1 -> RWSResult s a2 w2) -> RWS r w1 s a1 -> RWS r w2 s a2
```

#### `withRWS`

``` purescript
withRWS :: forall r1 r2 w s a. (r2 -> s -> Tuple r1 s) -> RWS r1 w s a -> RWS r2 w s a
```

#### `map_`

``` purescript
map_ :: forall r w s a b. (a -> b) -> RWS r w s a -> RWS r w s b
```

#### `(|->)`

``` purescript
infixl 4 map_ as |->
```

#### `apply_`

``` purescript
apply_ :: forall r w s a b. Semigroup w => RWS r w s (a -> b) -> RWS r w s a -> RWS r w s b
```

#### `(~)`

``` purescript
infixl 4 apply_ as ~
```

#### `pure_`

``` purescript
pure_ :: forall r w s a. Monoid w => a -> RWS r w s a
```

#### `bind_`

``` purescript
bind_ :: forall r w s a b. Semigroup w => RWS r w s a -> (a -> RWS r w s b) -> RWS r w s b
```

#### `(>>-)`

``` purescript
infixl 1 bind_ as >>-
```

#### `tailRec_`

``` purescript
tailRec_ :: forall r w s a b. Monoid w => (a -> RWS r w s (Either a b)) -> a -> RWS r w s b
```

#### `reader`

``` purescript
reader :: forall r w s a. Monoid w => (r -> a) -> RWS r w s a
```

Reader

#### `ask`

``` purescript
ask :: forall r w s. Monoid w => RWS r w s r
```

#### `local`

``` purescript
local :: forall r w s a. (r -> r) -> RWS r w s a -> RWS r w s a
```

#### `writer`

``` purescript
writer :: forall r w s a. Tuple a w -> RWS r w s a
```

Writer

#### `listen`

``` purescript
listen :: forall r w s a. RWS r w s a -> RWS r w s (Tuple a w)
```

#### `pass`

``` purescript
pass :: forall r w s a. RWS r w s (Tuple a (w -> w)) -> RWS r w s a
```

#### `tell`

``` purescript
tell :: forall r w s. w -> RWS r w s Unit
```

#### `listens`

``` purescript
listens :: forall r w s a b. (w -> b) -> RWS r w s a -> RWS r w s (Tuple a b)
```

#### `censor`

``` purescript
censor :: forall r w s a. Monoid w => (w -> w) -> RWS r w s a -> RWS r w s a
```

#### `state`

``` purescript
state :: forall r w s a. Monoid w => (s -> Tuple a s) -> RWS r w s a
```

State

#### `get`

``` purescript
get :: forall r w s. Monoid w => RWS r w s s
```

#### `gets`

``` purescript
gets :: forall r w s a. Monoid w => (s -> a) -> RWS r w s a
```

#### `put`

``` purescript
put :: forall r w s. Monoid w => s -> RWS r w s Unit
```

#### `modify`

``` purescript
modify :: forall r w s. Monoid w => (s -> s) -> RWS r w s Unit
```


