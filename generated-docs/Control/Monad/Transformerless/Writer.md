## Module Control.Monad.Transformerless.Writer

#### `Writer`

``` purescript
newtype Writer w a
  = Writer (Tuple a w)
```

##### Instances
``` purescript
Newtype (Writer w a) _
Functor (Writer w)
(Semigroup w) => Apply (Writer w)
(Monoid w) => Applicative (Writer w)
(Semigroup w) => Bind (Writer w)
(Monoid w) => Monad (Writer w)
```

#### `runWriter`

``` purescript
runWriter :: forall w a. Writer w a -> Tuple a w
```

#### `execWriter`

``` purescript
execWriter :: forall w a. Writer w a -> w
```

#### `mapWriter`

``` purescript
mapWriter :: forall w1 w2 a b. (Tuple a w1 -> Tuple b w2) -> Writer w1 a -> Writer w2 b
```

#### `mapW`

``` purescript
mapW :: forall w a b. (a -> b) -> Writer w a -> Writer w b
```

#### `(|->)`

``` purescript
infixl 4 mapW as |->
```

#### `applyW`

``` purescript
applyW :: forall w a b. Semigroup w => Writer w (a -> b) -> Writer w a -> Writer w b
```

#### `(~)`

``` purescript
infixl 4 applyW as ~
```

#### `pureW`

``` purescript
pureW :: forall w a. Monoid w => a -> Writer w a
```

#### `bindW`

``` purescript
bindW :: forall w a b. Semigroup w => Writer w a -> (a -> Writer w b) -> Writer w b
```

#### `(>>-)`

``` purescript
infixl 1 bindW as >>-
```

#### `pass`

``` purescript
pass :: forall w a. Writer w (Tuple a (w -> w)) -> Writer w a
```

#### `listen`

``` purescript
listen :: forall w a. Writer w a -> Writer w (Tuple a w)
```

#### `tell`

``` purescript
tell :: forall w. w -> Writer w Unit
```

#### `listens`

``` purescript
listens :: forall w a b. Monoid w => (w -> b) -> Writer w a -> Writer w (Tuple a b)
```

#### `censor`

``` purescript
censor :: forall w a. Monoid w => (w -> w) -> Writer w a -> Writer w a
```


