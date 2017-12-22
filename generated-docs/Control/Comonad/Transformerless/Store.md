## Module Control.Comonad.Transformerless.Store

#### `Store`

``` purescript
newtype Store s a
  = Store (Tuple (s -> a) s)
```

##### Instances
``` purescript
Newtype (Store s a) _
Functor (Store s)
Extend (Store s)
Comonad (Store s)
```

#### `runStore`

``` purescript
runStore :: forall s a. Store s a -> Tuple (s -> a) s
```

#### `store`

``` purescript
store :: forall s a. Tuple (s -> a) s -> Store s a
```

#### `peek`

``` purescript
peek :: forall s a. s -> Store s a -> a
```

1. Law: peek (pos x) x = extract x
2. Proof:
3. RHS := extract (f, s) = f s
4. LHS := peek (pos (f, s)) (f, s) =
5. peek s (f, s) =
6. peek s (f, _) = f s

#### `pos`

``` purescript
pos :: forall s a. Store s a -> s
```

1. Law: pos (extend _ x) = pos x
2. Proof:
3. RHS := pos (_, s)
4. LHS := pos (extend _ (f, s)) =
5. pos (extend _ (_, s)) =
6. pos (_, s)


