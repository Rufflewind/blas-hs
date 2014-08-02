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

The package is [available on Hackage][hac] and can thus be installed via Cabal
in the usual way:

    cabal install blas-hs

Before you dive in, however, be sure read up on the section
[Linking to Blas](#linking-to-blas).

Interface
---------

The package provides two complete interfaces:

- The `Blas.Primitive.*` modules expose the raw C interface.  All functions
  are named in the same fashion as the original Blas interface with the
  `cblas_` prefix removed.  This interface is stable.

- The `Blas.Generic.*` modules expose a slightly more sophisticated interface
  via type classes, allowing the functions to work on any of the 4 types
  supported by Blas.  This interface is still somewhat experimental.

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
CBlas binding from Netlib to remedy this:
http://netlib.org/blas/blast-forum/cblas.tgz

Linking to Blas
---------------

Linking blas-hs to the Blas library directly is optional: if you avoid
linking while building blas-hs, then the linking is deferred to the user
of the blas-hs library instead.  By default, blas-hs *does not
directly link to the Blas library*.

To link to the Blas library, you will need to set the appropriate flags in the
`*.cabal` file, which varies wildly depending on the implementation.  For the
reference Blas from Netlib, simply set:

    extra-libraries: blas

For other implementations, consult the relevant documentation on linker flags.

[ca]:  https://travis-ci.org/Rufflewind/blas-hs
[ci]:  https://travis-ci.org/Rufflewind/blas-hs.svg?branch=master
[ref]: http://netlib.org/blas
[pvp]: https://gist.github.com/Rufflewind/03f4e03f7cfa52b8f07d
[hac]: https://hackage.haskell.org/package/blas-hs
