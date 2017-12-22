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

#### `track`

``` purescript
track :: forall a m. Monoid m => m -> Traced m a -> a
```

1. Law: track mempty = extract
2. Proof:
3. First, rewrite as: track mempty f = extract f
4. RHS := f mempty
5. LHS := f mempty
6. Law: (track s =<= track t) x = track (s <> t) x
7. Proof:
8. RHS := track (s <> t) x = x (s <> t)
9. LHS := composeCoKliesliFlipped (track s) (track t) x =
10. track s (track t <<= x) =
11. track s (extend (track t) x) =
12. track s (\ t' -> (track t) \ t'' -> x (t' <> t'')) =
13. track s (\ t' -> x (t' <> t)) =
14. x (s <> t)

#### `tracks`

``` purescript
tracks :: forall a m. Monoid m => (a -> m) -> Traced m a -> a
```

#### `listen`

``` purescript
listen :: forall a m. Traced m a -> Traced m (Tuple a m)
```

#### `listens`

``` purescript
listens :: forall a b m. (m -> b) -> Traced m a -> Traced m (Tuple a b)
```

#### `censor`

``` purescript
censor :: forall a m. (m -> m) -> Traced m a -> Traced m a
```


