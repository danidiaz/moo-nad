# moo-nad

In [this Stack Overflow
question](https://stackoverflow.com/questions/61642492/simplifying-the-invocation-of-functions-stored-inside-an-readert-environment),
I asked how to simplify the invocation of functions stored in a `ReaderT`
environment.

For example, when invoking a `Int -> String -> _ ()` logging function from the environment, I would
like to simply be able to write:

    logic :: ReaderT EnvWithLogger IO ()
    logic = do
        self logger 7 "this is a message"

instead of something like

    logic :: ReaderT EnvWithLogger IO ()
    logic = do
        e <- ask
        liftIO $ logger e 7 "this is a message"

(Yes, I'm aware that this isn't *that* big of a hassle, and that solving it might
overcomplicate other things. But bear with me.)

The question received [this
answer](https://stackoverflow.com/a/61642757/1364288), which worked like a
charm. The answer also included the following comment:

> Implementing variadics with type classes is generally frowned upon because of
> how fragile they are, but it works well here because the RIO type provides a
> natural base case

That got me thinking: is there a way to avoid tying the workings of the
helper typeclass to a *concrete* monad, like
[`RIO`](http://hackage.haskell.org/package/rio)? Can the call-helper code be
made to work with a variety of reader-like monads?

After [a number of failed attempts](https://github.com/danidiaz/dep-t/issues/1)
using a typeclass-only approach, I turned to the solution explored in the
current repo: *abstract the monad and the environment using a [module
signature](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/separate_compilation.html#module-signatures)*.

That signature is called [`Moo`](./lib/Moo.hsig), and the module [`Moo.Prelude`](./lib/Moo/Prelude.hs) provides the
`self` and `call` helper methods.

## How to use this library to write program logic that is polymorphic on the monad and the environment?

This is an alternative to the usual way of abstracting the monad using [mtl](http://hackage.haskell.org/package/mtl).

Put program logic into indefinite libraries which depend
on the [`Moo` module signature](./lib/Moo.hsig). Import [`Moo.Prelude`](./lib/Moo/Prelude.hs) for the call helpers.

You'll likely need to expand the base `Moo` signature through [signature
merging](https://github.com/danidiaz/really-small-backpack-example/tree/master/lesson3-signature-merging) to require extra capabilities from the monad and/or the environment.

(**Note**: this approach is less fine-grained with respect to constraints than
the MTL one. When using MTL each individual function can have different
constraints. But here, functions from modules that import the same version of
`Moo` will share the same constraints. If you want constraint differentiation,
you'll need to create separate compilation units with different "enriched"
versions of `Moo`.)

You'll eventually need to write an implementation library that gives concrete instantiations for the monad and the environment.

In your executable, depend on both your program logic and the implementation library. The magic of [mixing matching](https://github.com/danidiaz/really-small-backpack-example/tree/master/lesson2-signatures) will take place, and you'll end up with a concrete version of your logic.

## Very well; how does an actual example look like?

- See the [example-logic-that-logs](./lib-example-logic-that-logs) internal library for an example of [abstract program logic](./lib-example-logic-that-logs/LogicThatLogs.hs) that imports an [enriched](./lib-example-logic-that-logs/Moo.hsig) version of `Moo`. 

- See also the [example-impl](./lib-example-impl) internal library that implements the `Moo` signature.

- The [test suite](./test/tests.hs) creates an actual concrete environment and runs the program logic with it.

Because we are using Backpack, we need to look at how everything is wired together
in the [cabal file](./moo-nad.cabal). Notice in particular how: 

- The program logic depends on `moo-nad` but *not* on the implementation.

- The implementation *doesn't* depend on `moo-nad`. Implementations in Backpack don't depend on the signatures they implement.

- The test suite depends on the program logic and the implementation.

## caveat emptor

At the end of the day, this method might involve too much ceremony to be practical. 

Feedback welcome. 

