# moo-nad

In [this Stack Overflow
question](https://stackoverflow.com/questions/61642492/simplifying-the-invocation-of-functions-stored-inside-an-readert-environment),
I asked how to simplify the invocation of functions stored in a `ReaderT`
environment.

For example, when invoking a [`logger`](./lib-example-logic-that-logs/LogicThatLogs.hs) function from the [environment](./lib-example-logic-that-logs/Moo.hsig), I would
like to simply be able to write:

    logic :: M ()
    logic = do
        call logger "this is a message"
        call logger "this is another message"

I received [this answer](https://stackoverflow.com/a/61642757/1364288), which
worked fine. The answer also included the following comment:

> Implementing variadics with type classes is generally frowned upon because of
> how fragile they are, but it works well here because the RIO type provides a
> natural base case

That got me thinking: is there a way to avoid tying the workings of the
typeclass to a *concrete* monad, like
[`RIO`](http://hackage.haskell.org/package/rio)? Can the call-helper code be
made to work with a variety of reader-like monads?

After [a number of failed attempts](https://github.com/danidiaz/dep-t/issues/1)
using a typeclass-only approach, I turned to the solution explored in this
repo: *abstract the monad which serves as the base case using a [module
signature](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/separate_compilation.html#module-signatures)*.

That signature is called `Moo`, and the module `Moo.Prelude` provides the
`call` helper method.

We could concievably put program logic into indefinite libraries which depend
on the `Moo` signature (perhaps expanded through [signature
merging](https://github.com/danidiaz/really-small-backpack-example/tree/master/lesson3-signature-merging)).
This would be a way of avoid depending on concrete monads—an alternative to
MTL, which solves a similar problem.

In practice though, this method might involve too much ceremony.

