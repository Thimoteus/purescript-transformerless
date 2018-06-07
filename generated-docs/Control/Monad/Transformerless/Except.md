## Module Control.Monad.Transformerless.Except

#### `Except`

``` purescript
newtype Except e a
  = Except (Either e a)
```

##### Instances
``` purescript
Newtype (Except e a) _
Invariant (Except e)
Bifunctor Except
Functor (Except e)
Apply (Except e)
Applicative (Except e)
Bind (Except e)
Monad (Except e)
Extend (Except e)
(Eq e, Eq a) => Eq (Except e a)
(Eq e) => Eq1 (Except e)
(Ord e, Ord a) => Ord (Except e a)
(Ord e) => Ord1 (Except e)
(Bounded e, Bounded a) => Bounded (Except e a)
Foldable (Except e)
Bifoldable Except
Traversable (Except e)
Bitraversable Except
(Semigroup a) => Semigroup (Except e a)
(Monoid e) => Alternative (Except e)
(Monoid e) => MonadZero (Except e)
(Monoid e) => MonadPlus (Except e)
(Show e, Show a) => Show (Except e a)
(Semigroup e) => Alt (Except e)
(Monoid e) => Plus (Except e)
```

#### `runExcept`

``` purescript
runExcept :: forall e. (Except e) ~> (Either e)
```

#### `withExcept`

``` purescript
withExcept :: forall e1 e2. (e1 -> e2) -> (Except e1) ~> (Except e2)
```

#### `mapExcept`

``` purescript
mapExcept :: forall e1 e2 a1 a2. (Either e1 a1 -> Either e2 a2) -> Except e1 a1 -> Except e2 a2
```

#### `throwError`

``` purescript
throwError :: forall e a. e -> Except e a
```

#### `catchError`

``` purescript
catchError :: forall e a. Except e a -> (e -> Except e a) -> Except e a
```

#### `except`

``` purescript
except :: forall e a. Either e a -> Except e a
```


