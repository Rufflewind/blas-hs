Haskell interface to Blas
=========================

This package provides a complete low-level binding to Blas via the foreign
function interface, allowing Haskell programs to take advantage of optimized
routines for vector and matrix operations in Haskell.  For more information
about Blas, see http://netlib.org/blas

The bindings are compatible with any library that implements the standard C
interface for Blas.  If your implementation does not provide a C interface or
uses a nonstandard interface, consider installing the CBlas binding from
Netlib: http://netlib.org/blas/blast-forum/cblas.tgz

Documentation is available [here](http://rufflewind.com/blas-hs).

Interface
---------

The package provides two complete interfaces:

- The `Blas.Primitive.*` modules expose the raw C interface.  All functions
  are named in the same fashion as the original Blas interface with the prefix
  removed.  This interface is stable.

- The `Blas.Generic.*` modules expose a slightly more sophisticated interface
  via type classes, allowing the functions to work on any of the 4 types
  supported by Blas.  This interface is still somewhat experimental.

Both of these two interfaces have `Safe` and `Unsafe` versions, of which the
only difference is the type of foreign call used.  Refer to the GHC
documentation for more information regarding appropriate use of safe and
unsafe foreign calls.
