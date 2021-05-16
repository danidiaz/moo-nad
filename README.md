# moo-nad

In [this Stack Overflow
question](https://stackoverflow.com/questions/61642492/simplifying-the-invocation-of-functions-stored-inside-an-readert-environment),
I asked how to simplify the invocation of functions stored in a `ReaderT`
environment.

I received [this answer](https://stackoverflow.com/a/61642757/1364288), which
worked fine. The also also included this comment:

> Implementing variadics with type classes is generally frowned upon because of
> how fragile they are, but it works well here because the RIO type provides a
> natural base case

That got me thinking: is there a way to avoid tying the workings of the
typeclass to a *concrete* monad, like `RIO`? Can the call-helper code be made
to work with a variety of monads?

After [a number of failed attempts](https://github.com/danidiaz/dep-t/issues/1)
using a typeclass-only approach, I turned to the solution explored in this
repo: *abstract the monad which serves as a base case using a module
signature*.

That signature is called `Moo`, and the module `Moo.Prelude` provides the
invocation helper typeclass.


