blas-hs [![Build status][ci]][ca]
=================================

**blas-hs** provides a complete low-level interface to Blas via the foreign
function interface, allowing Haskell programs to take advantage of optimized
routines for vector and matrix operations in Haskell.  For more information
about Blas, see http://netlib.org/blas

blas-hs is compatible with any library that implements the standard C
interface for Blas.  If your implementation does not provide a C interface or
uses a nonstandard interface, consider installing the CBlas binding from
Netlib: http://netlib.org/blas/blast-forum/cblas.tgz

Documentation is [available on Hackage][hac].

Take note of the [versioning policy of this package][pvp].

Quick start
-----------

Before you install, be sure to read up on
[how linking is handled](#linking-to-blas).  The linking process is *not fully
automated* and may require some manual intervention depending on your system
configuration.

Since package is [available on Hackage][hac], it can be installed via
cabal-install in the usual way:

```sh
cabal install blas-hs
```

Interface
---------

The package provides two complete interfaces:

- The `Blas.Primitive.*` modules expose the raw C interface.  All functions
  are named in the same fashion as the original Blas interface with the
  `cblas_` prefix removed.  This interface is stable.

- The `Blas.Generic.*` modules expose a slightly more sophisticated interface
  via type classes, allowing the functions to work on any of the 4 types
  supported by Blas.  This interface is experimental.

- The `Blas.Specialized.*` modules expose a more uniform interface similar to
  `Blas.Generic.*` but without any type classes.  This interface is
  experimental.

Both of these two interfaces have `Safe` and `Unsafe` versions, of which the
only difference is the type of foreign call used.  Refer to the GHC
documentation for more information regarding appropriate use of safe and
unsafe foreign calls.

Requirements
------------

blas-hs requires either the
[reference implementation of Blas from Netlib][ref] or any alternative
implementation of Blas that is compliant with the *de facto* Blas interface
(e.g. ACML, ATLAS, OpenBLAS, MKL, and many others).  In addition, the
implementation must supply a C interface (*not* the Fortran one).
Fortunately, as long as the Fortran interface is available, one can use the
CBlas bindings from Netlib to remedy this:
http://netlib.org/blas/blast-forum/cblas.tgz

Linking to Blas
---------------

By default, blas-hs will link using `-lblas`.  On Darwin or Mac OS X, blas-hs
will instead use the Accelerate framework.

For any other configuration, you will need to
[tweak this process](#predefined-flags).

Note that linking blas-hs to the Blas library directly is optional: if you
avoid linking while building blas-hs, then the linking is deferred to the user
of the blas-hs library instead.

### Predefined flags

The `blas-hs.cabal` file provides a set of predefined flags to cover a few of
the common use cases.  If you want to do something more exotic, you may need
to [edit the `blas-hs.cabal` file directly](#custom-configuration).

To override the default settings, you will need to give additional arguments
to cabal-install:

<pre><code>cabal install blas-hs <var>FLAGS</var>...
# note: also works for `cabal configure`</code></pre>

Each flag is preceded by `-f`, so to turn on a flag named `openblas`, you
would specify `-fopenblas`.  To turn *off* the flag, specify `-f-openblas`
instead.

The flags here are listed in order of precedence: each flag will *override*
all preceding flags, so if you (for example) turn both `openblas` and `mkl`
on, then `mkl` takes precedence.  All flags are *off* by default.

  - `no-netlib`: Do *not* link with the reference Blas from Netlib.
  - `no-accelerate`: Do *not* link with the Accelerate Framework.  Has no
    effect except on Darwin / Mac OS X.
  - `openblas`: Link with the OpenBLAS library.
  - `mkl`: Link with the Intel Math Kernel Library.

Additionally, there is also the `cblas` flag that can be used to link to the
CBlas bindings, in case your Blas implementation doesn't provide a C
interface.  This flag may be used in conjunction with any of the other flags.

### Custom configuration

In a more complicated situation, you will have to modify the `blas-hs.cabal`
file.  First, you need to figure out which libraries to link to, as well as
any additional options that need to be passed to the linker.  This usually
comes as a set of command-line arguments.  For example,

    -fopenmp -L/usr/local/foobar/lib -lfoo -lbar

The exact meaning of the flags can be found in the
[manual of ld](https://sourceware.org/binutils/docs/ld/Options.html).  For our
purposes we only care about two kinds: those preceded by `-l` and those that
aren't.  The ones preceded by `-l` will go into the `extra-libraries` entry
(without the `-l` prefix), while those that aren't will go into the
`ld-options` entry.

Now it's just a matter of modifying the package.

 1. Download the package:

    ```sh
    cabal get blas-hs
    ```

 2. Edit the `blas-hs.cabal` to add the custom configuration:

          -- custom configuration
          extra-libraries: foo bar
          ld-options:      -fopenmp -L/usr/local/foobar/lib

 3. Change the current directory to that of the package, then run

    ```sh
    cabal install -fno-netlib -fno-accelerate
    ```

[ca]:  https://github.com/Rufflewind/blas-hs/actions/workflows/build.yml
[ci]:  https://github.com/Rufflewind/blas-hs/actions/workflows/build.yml/badge.svg
[ref]: http://netlib.org/blas
[pvp]: https://gist.github.com/Rufflewind/03f4e03f7cfa52b8f07d
[hac]: https://hackage.haskell.org/package/blas-hs
