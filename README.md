# purescript-transformerless

## Why?
In Haskell and Purescript, the standard `Writer`, `Reader`, `State` and `RWS`
monads are implemented in terms of their monad *transformer* versions over the
`Identity` monad. Depending on how you learned about monad transformers, you might
remember reading something like the following:

> The State/Reader/Writer monad is ...
>
> A monad transformer is ...
>
> The StateT/ReaderT/WriterT monad transformer is ...
>
> In fact, the State/Reader/Writer monad from section {3 lines ago} is actually
defined as StateT s Identity/ReaderT r Identity/WriterT w Identity!

Wow, what a plot twist!

However, for all the theoretical cleanliness, it's "common knowledge" among
Purescripters that transformer stacks are slow and generate some funky Javascript.

## Usage

The same as a normal `State`, `Reader`, etc. However, you should know that
none of these types have instances for their respective transformer counterparts:
there is no instance for `..State.Class.MonadState s (..Transformerless.State s)`
or its buddies. Wouldn't it be weird for a package called "transformerless" to
depend on a package called "transformers"?

As a result, a "transformers" typeclass function is just a normal function in
the transformerless counterpart's module.

## Scrap Your Typeclasses

Each module also exports normal functions corresponding to typeclass members for
each typeclass instance in the module.

`Control.Monad.Transformerless.Reader` exports `mapR, applyR, pureR, bindR`
as well as infix aliases `|->, ~, >>-` for `mapR, applyR, bindR` respectively.

`Writer` and `State` are similar, but `RWS` exports `map_, apply_, pure_, bind_`.
However, the aliases are the same in each module.

Using these instead of their overloaded versions avoids passing typeclass
dictionaries, and could result in a speedup.

## Questions I'll Ask and Answer for You

### You mentioned code generation. Is it really better for this package?

Examples:

Generating code for this transformers code:
```purescript
loop :: Int -> RWST String (Array String) Int Identity Unit
loop n = tailRecM go n
  where
  go 0 = do
    tell [ "Done!" ]
    pure (Right unit)
  go n = do
    x <- get
    put (x + 1)
    pure (Left (n - 1))
```

results in this javascript:

```javascript
var loop = function (n) {
    var go = function (v) {
        if (v === 0) {
            return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_RWS_Trans.bindRWST(Data_Identity.bindIdentity)(Data_Monoid.monoidArray))(Control_Monad_Writer_Class.tell(Control_Monad_RWS_Trans.monadTellRWST(Data_Identity.monadIdentity)(Data_Monoid.monoidArray))([ "Done!" ]))(function () {
                return Control_Applicative.pure(Control_Monad_RWS_Trans.applicativeRWST(Data_Identity.monadIdentity)(Data_Monoid.monoidArray))(new Control_Monad_Rec_Class.Done(Data_Unit.unit));
            });
        };
        return Control_Bind.bind(Control_Monad_RWS_Trans.bindRWST(Data_Identity.bindIdentity)(Data_Monoid.monoidArray))(Control_Monad_State_Class.get(Control_Monad_RWS_Trans.monadStateRWST(Data_Identity.monadIdentity)(Data_Monoid.monoidArray)))(function (v1) {
            return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_RWS_Trans.bindRWST(Data_Identity.bindIdentity)(Data_Monoid.monoidArray))(Control_Monad_State_Class.put(Control_Monad_RWS_Trans.monadStateRWST(Data_Identity.monadIdentity)(Data_Monoid.monoidArray))(v1 + 1 | 0))(function () {
                return Control_Applicative.pure(Control_Monad_RWS_Trans.applicativeRWST(Data_Identity.monadIdentity)(Data_Monoid.monoidArray))(new Control_Monad_Rec_Class.Loop(v - 1 | 0));
            });
        });
    };
    return Control_Monad_Rec_Class.tailRecM(Control_Monad_RWS_Trans.monadRecRWST(Control_Monad_Rec_Class.monadRecIdentity)(Data_Monoid.monoidArray))(go)(n);
};
```

vs this transformerless code:
```purescript
loop :: Int -> RWS.RWS String (Array String) Int Unit
loop n = RWS.tailRec_ go n where
  go 0 = do
    _ <- RWS.tell ["Done!"]
    RWS.pure_ (Done unit)
      where
        bind = RWS.bind_
  go m = do
    x <- RWS.get
    _ <- RWS.put (x + 1)
    RWS.pure_ (Loop (m - 1))
      where
        bind = RWS.bind_
```

with this javascript:
```javascript
var loop = function (n) {
    var go = function (v) {
        if (v === 0) {
            return Control_Monad_Transformerless_RWS.bind_(Data_Semigroup.semigroupArray)(Control_Monad_Transformerless_RWS.tell([ "Done!" ]))(function (v1) {
                return Control_Monad_Transformerless_RWS.pure_(Data_Monoid.monoidArray)(new Control_Monad_Rec_Class.Done(Data_Unit.unit));
            });
        };
        return Control_Monad_Transformerless_RWS.bind_(Data_Semigroup.semigroupArray)(Control_Monad_Transformerless_RWS.get(Data_Monoid.monoidArray))(function (v1) {
            return Control_Monad_Transformerless_RWS.bind_(Data_Semigroup.semigroupArray)(Control_Monad_Transformerless_RWS.put(Data_Monoid.monoidArray)(v1 + 1 | 0))(function (v2) {
                return Control_Monad_Transformerless_RWS.pure_(Data_Monoid.monoidArray)(new Control_Monad_Rec_Class.Loop(v - 1 | 0));
            });
        });
    };
    return Control_Monad_Transformerless_RWS.tailRec_(Data_Monoid.monoidArray)(go)(n);
};
```

### What about speed?

On my computer, testing the above `loop` functions 10,000,000 times. The transformerless version is on the right, and
timing the `loop` function is labeled "RWS". The transformer version is labeled "RWST":

![test](http://i.imgur.com/Fww56is.png)

And if you can't read my font, it says `RWST: 38929.0` and `RWS: 15048.0`.

## Installing
`bower i purescript-transformerless`
