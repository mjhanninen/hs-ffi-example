# A Haskell FFI Example #

This is a small but working example on using the Haskell FFI to interface with
a C library. In particular this example sports:

- operating with side-effecting C code

- handling opaque pointers

- passing around data in structs


## Description ##

The C library of this example interfaces with a fictitious device that gives
you a reading of some value when it doesn't flake. The normal operation is the
following:

1. You acquire a context within which the measurements are made by calling
   the `alloc_context` API function. The allocation may fail.

2. You do your measurements by calling the `measure` API function one or more
   times. Any of the calls my fail.

3. Finally you release the context explicitly by calling the `release_context`
   API function. This may fail too.

The library is designed so that:

- the private data are hidden inside an opaque pointer (i.e. the context)

- the return value indicates only whether the call was successful (i.e. 0)

- the real outbound data flow happen through pointer arguments.

The Haskell side exposes types `Context` and `Measurement` that
correspondingly represents the measurement context and an actual
measurement. Instead of acquiring and releasing the context separately, like
you're forced to do with the C API, you are expected do all your measurements
in a function of type `Context -> IO a` and treat context as given. Then you
pass that fuction to `withContext` of type `(Context -> IO a) -> IO (Maybe a)`
that allocates the context, runs the actions in your function, and releases
the context before exiting the scope. Individual measurement is done by
calling `measure` of type `Context -> IO (Maybe Measurement)`.


## Running the example ##

To run the example:

    $ make all
    $ ./TestFrobnicator

There is also a C-only tester `test_frobnicator` but it is mainly there for
troubleshooting.

I have tested the example with GHC 7.6.3 running on OS X 10.9 and installed
with the Homebrew.


## Further resources ##

I wrote example this in order figure out how to use FFI in my other project.
While researching the topic I found the following resources very useful:

- Rebecca Skinner's slides ["Understanding the Haskell FFI"][skinner2013].

- Magnus Therning's blog post ["Haskell and C structs"][therning2007].

- The ["Haskell/FFI"][wikibook] chapter in the Haskell Wikibook.

[skinner2013]: http://www.rebeccaskinner.net/assets/haskell_ffi/understanding_the_haskell_ffi.pdf
    "Skinner, Rebecca: Understanding The Haskell FFI"

[therning2007]: http://therning.org/magnus/archives/315
    "Therning, Magnus: Haskell and C structs"

[wikibook]: https://en.wikibooks.org/wiki/Haskell/FFI
    "Wikibooks: Haskell/FFI"
