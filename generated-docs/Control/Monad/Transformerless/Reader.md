## Module Control.Monad.Transformerless.Reader

#### `Reader`

``` purescript
newtype Reader r a
  = Reader (r -> a)
```

##### Instances
``` purescript
Newtype (Reader r a) _
Functor (Reader r)
Apply (Reader r)
Applicative (Reader r)
Bind (Reader r)
Monad (Reader r)
```

#### `runReader`

``` purescript
runReader :: forall r a. Reader r a -> r -> a
```

#### `withReader`

``` purescript
withReader :: forall r1 r2 a. (r2 -> r1) -> Reader r1 a -> Reader r2 a
```

#### `mapReader`

``` purescript
mapReader :: forall r a b. (a -> b) -> Reader r a -> Reader r b
```

#### `(|->)`

``` purescript
infixl 4 mapReader as |->
```

#### `applyR`

``` purescript
applyR :: forall r a b. Reader r (a -> b) -> Reader r a -> Reader r b
```

#### `(~)`

``` purescript
infixl 4 applyR as ~
```

#### `pureR`

``` purescript
pureR :: forall r a. a -> Reader r a
```

#### `bindR`

``` purescript
bindR :: forall r a b. Reader r a -> (a -> Reader r b) -> Reader r b
```

#### `(>>-)`

``` purescript
infixl 1 bindR as >>-
```

#### `local`

``` purescript
local :: forall r a. (r -> r) -> Reader r a -> Reader r a
```

#### `ask`

``` purescript
ask :: forall r. Reader r r
```


