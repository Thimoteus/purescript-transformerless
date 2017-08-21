## Module Control.Comonad.Transformerless.Env

#### `Env`

``` purescript
newtype Env e a
  = Env (Tuple e a)
```

##### Instances
``` purescript
Newtype (Env e a) _
Functor (Env e)
Extend (Env e)
Comonad (Env e)
```

#### `runEnv`

``` purescript
runEnv :: forall e a. Env e a -> Tuple e a
```

#### `withEnv`

``` purescript
withEnv :: forall e1 e2 a. (e1 -> e2) -> Env e1 a -> Env e2 a
```

#### `mapEnv`

``` purescript
mapEnv :: forall a b e. (a -> b) -> Env e a -> Env e b
```

#### `env`

``` purescript
env :: forall a e. e -> a -> Env e a
```

#### `ask`

``` purescript
ask :: forall a e. Env e a -> e
```

#### `asks`

``` purescript
asks :: forall a e1 e2. (e1 -> e2) -> Env e1 a -> e2
```

#### `local`

``` purescript
local :: forall e a. (e -> e) -> Env e a -> Env e a
```


