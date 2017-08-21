## Module Control.Comonad.Transformerless.Traced

#### `Traced`

``` purescript
newtype Traced m a
  = Traced (m -> a)
```

##### Instances
``` purescript
Newtype (Traced w a) _
Functor (Traced m)
(Semigroup m) => Extend (Traced m)
(Monoid m) => Comonad (Traced m)
```

#### `runTraced`

``` purescript
runTraced :: forall m a. Traced m a -> m -> a
```

#### `traced`

``` purescript
traced :: forall m a. (m -> a) -> Traced m a
```


