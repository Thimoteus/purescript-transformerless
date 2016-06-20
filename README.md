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

The same as a normal `State`, `Reader`, etc. monad. However, you should know that
none of these monads have instances for their respective transformer counterparts:
there is no instance for `..State.Class.MonadState s (..Transformerless.State s)`
or its buddies. Wouldn't it be weird for a package called "transformerless" to
depend on a package called "transformers"?

As a result, a "transformers" typeclass function is just a normal function in
the transformerless counterpart's module.

## Questions I'll Ask and Answer for You

### You mentioned code generation. Is it really better for this package?

Examples:

Generating code for this:
```purescript
loop :: Int -> RWS String (Array String) Int Unit
loop n = tailRecM go n where
  go 0 = do
    RWS.tell ["Done!"]
    pure (Right unit)
  go n = do
    x <- RWS.get
    RWS.put (x + 1)
    pure (Left (n - 1))
```

with transformers:
```javascript
var loop = function (n) {
    var go = function (v) {
        if (v === 0) {
            return Control_Bind.bind(Control_Monad_RWS_Trans.bindRWST(Data_Identity.bindIdentity)(Data_Monoid.monoidArray))(Control_Monad_Writer_Class.tell(Control_Monad_RWS_Trans.monadWriterRWST(Data_Identity.monadIdentity)(Data_Monoid.monoidArray))([ "Done!" ]))(function () {
                return Control_Applicative.pure(Control_Monad_RWS_Trans.applicativeRWST(Data_Identity.monadIdentity)(Data_Monoid.monoidArray))(new Data_Either.Right(Data_Unit.unit));
            });
        };
        return Control_Bind.bind(Control_Monad_RWS_Trans.bindRWST(Data_Identity.bindIdentity)(Data_Monoid.monoidArray))(Control_Monad_State_Class.get(Control_Monad_RWS_Trans.monadStateRWST(Data_Identity.monadIdentity)(Data_Monoid.monoidArray)))(function (v1) {
            return Control_Bind.bind(Control_Monad_RWS_Trans.bindRWST(Data_Identity.bindIdentity)(Data_Monoid.monoidArray))(Control_Monad_State_Class.put(Control_Monad_RWS_Trans.monadStateRWST(Data_Identity.monadIdentity)(Data_Monoid.monoidArray))(v1 + 1 | 0))(function () {
                return Control_Applicative.pure(Control_Monad_RWS_Trans.applicativeRWST(Data_Identity.monadIdentity)(Data_Monoid.monoidArray))(new Data_Either.Left(v - 1));
            });
        });
    };
    return Control_Monad_Rec_Class.tailRecM(Control_Monad_RWS_Trans.monadRecRWST(Control_Monad_Rec_Class.monadRecIdentity)(Data_Monoid.monoidArray))(go)(n);
};
```

without transformers:
```javascript
var loop = function (n) {
    var go = function (v) {
        if (v === 0) {
            return Control_Bind.bind(Control_Monad_Transformerless_RWS.bindRWS(Data_Semigroup.semigroupArray))(Control_Monad_Transformerless_RWS.tell([ "Done!" ]))(function () {
                return Control_Applicative.pure(Control_Monad_Transformerless_RWS.applicativeRWS(Data_Monoid.monoidArray))(new Data_Either.Right(Data_Unit.unit));
            });
        };
        return Control_Bind.bind(Control_Monad_Transformerless_RWS.bindRWS(Data_Semigroup.semigroupArray))(Control_Monad_Transformerless_RWS.get(Data_Monoid.monoidArray))(function (v1) {
            return Control_Bind.bind(Control_Monad_Transformerless_RWS.bindRWS(Data_Semigroup.semigroupArray))(Control_Monad_Transformerless_RWS.put(Data_Monoid.monoidArray)(v1 + 1 | 0))(function () {
                return Control_Applicative.pure(Control_Monad_Transformerless_RWS.applicativeRWS(Data_Monoid.monoidArray))(new Data_Either.Left(v - 1));
            });
        });
    };
    return Control_Monad_Rec_Class.tailRecM(Control_Monad_Transformerless_RWS.monadRecRWS(Data_Monoid.monoidArray))(go)(n);
};
```

### And what about speed?

On my computer, testing the above `loop` function (which is duplicated both on the
transformers repo and this one). The transformerless version is on the left, and
timing the `loop` function is labeled "RWS". The transformer version is labeled "RWST":

![test](http://i.imgur.com/oL8Nqhg.png)

## Installing
`bower i purescript-transformerless`
