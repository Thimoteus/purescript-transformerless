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

Law: peek (pos x) x = extract x
Proof:
RHS := extract (f, s) = f s
LHS := peek (pos (f, s)) (f, s) = 
peek s (f, s) =
peek s (f, _) = f s

#### `pos`

``` purescript
pos :: forall s a. Store s a -> s
```

Law: pos (extend _ x) = pos x
Proof:
RHS := pos (_, s)
LHS := pos (extend _ (f, s)) =
pos (extend _ (_, s)) =
pos (_, s)


