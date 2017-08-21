## Module Control.Monad.Transformerless.Cont

#### `Cont`

``` purescript
newtype Cont r a
  = Cont ((a -> r) -> r)
```

##### Instances
``` purescript
Newtype (Cont r a) _
Functor (Cont r)
Apply (Cont r)
Applicative (Cont r)
Bind (Cont r)
Monad (Cont r)
```

#### `runCont`

``` purescript
runCont :: forall r a. Cont r a -> ((a -> r) -> r)
```

#### `cont`

``` purescript
cont :: forall a r. ((a -> r) -> r) -> Cont r a
```

#### `callCC`

``` purescript
callCC :: forall a r. ((forall b. a -> Cont r b) -> Cont r a) -> Cont r a
```

#### `mapCont`

``` purescript
mapCont :: forall r a. (r -> r) -> Cont r a -> Cont r a
```

#### `withCont`

``` purescript
withCont :: forall a b r. ((b -> r) -> (a -> r)) -> Cont r a -> Cont r b
```


