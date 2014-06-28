{-# LANGUAGE FlexibleInstances, TypeFamilies #-}
{-|

Copyright: (c) 2014 Phil Ruffwind
License: MIT
Maintainer: rf@rufflewind.com
Stability: experimental

Generic interface to Blas.

The functions here are named in a similar fashion to the original Blas
interface, with the type-dependent letter(s) removed.  Some functions have
been merged with others to allow the interface to work on both real and
complex numbers.  If you can't a particular function, try looking for its
corresponding complex equivalent (e.g. @symv@ is a special case of 'hemv'
applied to real numbers).

The generic interfaces uses either safe (@*.Unsafe@) or unsafe (@*.Unsafe@)
foreign calls.  Refer to the GHC documentation for more information regarding
appropriate use of safe and unsafe foreign calls.

It'd be really nice if these functions were actually documented.  Alas, for
the time being, you'll have to refer to the various Blas docs scattered across
the Internet.  Note that the interface is based off of /CBlas/, not Fortran
Blas, which is slightly different.

-}
module Blas.Generic.Unsafe
       ( Numeric(..)
       , RealNumeric(..)
       , C.dsdot
       , C.sdsdot
       ) where
import Data.Complex (Complex((:+)))
import Foreign (Ptr, Storable)
import FFI (getReturnValue)
import Blas.Primitive.Types (Order, Transpose, Uplo, Diag, Side)
import qualified Blas.Primitive.Unsafe as C

-- | Blas operations that are applicable to real and complex numbers.
--
--   Instances are defined for the 4 types supported by Blas: the
--   single- and double-precision floating point types and their complex
--   versions.
class (Floating a, Storable a) => Numeric a where

  -- | The corresponding real type of @a@.
  --
  --   In other words, @'RealType' ('Complex' a)@ is an alias for @a@.  For
  --   everything else, @'RealType' a@ is simply @a@.
  type RealType a :: *

  dotu  :: Int -> Ptr a -> Int -> Ptr a -> Int -> IO a
  dotc  :: Int -> Ptr a -> Int -> Ptr a -> Int -> IO a
  nrm2  :: Int -> Ptr a -> Int -> IO (RealType a)
  asum  :: Int -> Ptr a -> Int -> IO (RealType a)
  iamax :: Int -> Ptr a -> Int -> IO Int
  swap  :: Int -> Ptr a -> Int -> Ptr a -> Int -> IO ()
  copy  :: Int -> Ptr a -> Int -> Ptr a -> Int -> IO ()
  axpy  :: Int -> a -> Ptr a -> Int -> Ptr a -> Int -> IO ()
  scal  :: Int -> a -> Ptr a -> Int -> IO ()
  gemv  :: Order -> Transpose -> Int -> Int -> a -> Ptr a -> Int -> Ptr a
        -> Int -> a -> Ptr a -> Int -> IO ()
  gbmv  :: Order -> Transpose -> Int -> Int -> Int -> Int -> a -> Ptr a
        -> Int -> Ptr a -> Int -> a -> Ptr a -> Int -> IO ()
  trmv  :: Order -> Uplo -> Transpose -> Diag -> Int -> Ptr a -> Int
        -> Ptr a -> Int -> IO ()
  tbmv  :: Order -> Uplo -> Transpose -> Diag -> Int -> Int -> Ptr a
        -> Int -> Ptr a -> Int -> IO ()
  tpmv  :: Order -> Uplo -> Transpose -> Diag -> Int -> Ptr a -> Ptr a
        -> Int -> IO ()
  trsv  :: Order -> Uplo -> Transpose -> Diag -> Int -> Ptr a -> Int
        -> Ptr a -> Int -> IO ()
  tbsv  :: Order -> Uplo -> Transpose -> Diag -> Int -> Int -> Ptr a
        -> Int -> Ptr a -> Int -> IO ()
  tpsv  :: Order -> Uplo -> Transpose -> Diag -> Int -> Ptr a -> Ptr a
        -> Int -> IO ()
  hemv  :: Order -> Uplo -> Int -> a -> Ptr a -> Int -> Ptr a -> Int
        -> a -> Ptr a -> Int -> IO ()
  hbmv  :: Order -> Uplo -> Int -> Int -> a -> Ptr a -> Int -> Ptr a
        -> Int -> a -> Ptr a -> Int -> IO ()
  hpmv  :: Order -> Uplo -> Int -> a -> Ptr a -> Ptr a -> Int -> a
        -> Ptr a -> Int -> IO ()
  geru  :: Order -> Int -> Int -> a -> Ptr a -> Int -> Ptr a -> Int
        -> Ptr a -> Int -> IO ()
  gerc  :: Order -> Int -> Int -> a -> Ptr a -> Int -> Ptr a -> Int
        -> Ptr a -> Int -> IO ()
  her   :: Order -> Uplo -> Int -> RealType a -> Ptr a -> Int
        -> Ptr a -> Int -> IO ()
  hpr   :: Order -> Uplo -> Int -> RealType a -> Ptr a -> Int
        -> Ptr a -> IO ()
  her2  :: Order -> Uplo -> Int -> a -> Ptr a -> Int -> Ptr a -> Int
        -> Ptr a -> Int -> IO ()
  hpr2  :: Order -> Uplo -> Int -> a -> Ptr a -> Int -> Ptr a -> Int
        -> Ptr a -> IO ()

  -- | Calculate a general matrix-matrix product:
  --
  --   > c := alpha * opa(a) * opb(b) + beta * c
  --
  --   where `opa` and `opb` are operations specified by @transa@ and
  --   @transb@ respectively.
  gemm  :: Order     -- ^ Layout of all matrices.
        -> Transpose -- ^ (@transa@) Operation applied to @a@.
        -> Transpose -- ^ (@transb@) Operation applied to @b@.
        -> Int       -- ^ (@m@) Number of rows of @op(a)@ and @c@.
        -> Int       -- ^ (@n@) Number of columns of @op(b)@ and @c@.
        -> Int       -- ^ (@k@) Number of columns of @op(a)@ and
                     --         number of of rows of @op(b)@.
        -> a         -- ^ (@alpha@) Scaling factor of the product.
        -> Ptr a     -- ^ (@a@) Pointer to a matrix.
        -> Int       -- ^ (@lda@) Stride of the major dimension of @a@.
        -> Ptr a     -- ^ (@b@) Pointer to a matrix.
        -> Int       -- ^ (@ldb@) Stride of the major dimension of @b@.
        -> a         -- ^ (@beta@) Scaling factor of the original @c@.
        -> Ptr a     -- ^ (@c@) Pointer to a mutable matrix.
        -> Int       -- ^ (@ldc@) Stride of the major dimension of @c@.
        -> IO ()

  symm  :: Order -> Side -> Uplo -> Int -> Int -> a -> Ptr a -> Int
        -> Ptr a -> Int -> a -> Ptr a -> Int -> IO ()
  syrk  :: Order -> Uplo -> Transpose -> Int -> Int -> a -> Ptr a -> Int
        -> a -> Ptr a -> Int -> IO ()
  syr2k :: Order -> Uplo -> Transpose -> Int -> Int -> a -> Ptr a -> Int
        -> Ptr a -> Int -> a -> Ptr a -> Int -> IO ()
  hemm  :: Order -> Side -> Uplo -> Int -> Int -> a -> Ptr a -> Int
        -> Ptr a -> Int -> a -> Ptr a -> Int -> IO ()
  herk  :: Order -> Uplo -> Transpose -> Int -> Int -> RealType a -> Ptr a
        -> Int -> RealType a -> Ptr a -> Int -> IO ()
  her2k :: Order -> Uplo -> Transpose -> Int -> Int -> a -> Ptr a -> Int
        -> Ptr a -> Int -> RealType a -> Ptr a -> Int -> IO ()
  trmm  :: Order -> Side -> Uplo -> Transpose -> Diag -> Int -> Int
        -> a -> Ptr a -> Int -> Ptr a -> Int -> IO ()
  trsm  :: Order -> Side -> Uplo -> Transpose -> Diag -> Int -> Int
        -> a -> Ptr a -> Int -> Ptr a -> Int -> IO ()

-- | Blas operations that are only applicable to real numbers.
--
--   Note: although complex versions of 'rot' and 'rotg' exist in many
--   implementations, they are not part of the official Blas standard and
--   therefore not included here.  If you /really/ need them, submit a ticket
--   so we can try to come up with a solution.
class Numeric a => RealNumeric a where
  rotg  :: Ptr a -> Ptr a -> Ptr a -> Ptr a -> IO ()
  rotmg :: Ptr a -> Ptr a -> Ptr a -> a -> Ptr a -> IO ()
  rot   :: Int -> Ptr a -> Int -> Ptr a -> Int -> a -> a -> IO ()
  rotm  :: Int -> Ptr a -> Int -> Ptr a -> Int -> Ptr a -> IO ()

instance Numeric Float where
  type RealType Float = Float
  dotu  = C.sdot
  dotc  = C.sdot
  nrm2  = C.snrm2
  asum  = C.sasum
  iamax = C.isamax
  swap  = C.sswap
  copy  = C.scopy
  axpy  = C.saxpy
  scal  = C.sscal
  gemv  = C.sgemv
  gbmv  = C.sgbmv
  trmv  = C.strmv
  tbmv  = C.stbmv
  tpmv  = C.stpmv
  trsv  = C.strsv
  tbsv  = C.stbsv
  tpsv  = C.stpsv
  hemv  = C.ssymv
  hbmv  = C.ssbmv
  hpmv  = C.sspmv
  geru  = C.sger
  gerc  = C.sger
  her   = C.ssyr
  hpr   = C.sspr
  her2  = C.ssyr2
  hpr2  = C.sspr2
  gemm  = C.sgemm
  symm  = C.ssymm
  hemm  = C.ssymm
  syrk  = C.ssyrk
  herk  = C.ssyrk
  syr2k = C.ssyr2k
  her2k = C.ssyr2k
  trmm  = C.strmm
  trsm  = C.strsm

instance RealNumeric Float where
  rotg  = C.srotg
  rotmg = C.srotmg
  rot   = C.srot
  rotm  = C.srotm

instance Numeric Double where
  type RealType Double = Double
  dotu  = C.ddot
  dotc  = C.ddot
  nrm2  = C.dnrm2
  asum  = C.dasum
  iamax = C.idamax
  swap  = C.dswap
  copy  = C.dcopy
  axpy  = C.daxpy
  scal  = C.dscal
  gemv  = C.dgemv
  gbmv  = C.dgbmv
  trmv  = C.dtrmv
  tbmv  = C.dtbmv
  tpmv  = C.dtpmv
  trsv  = C.dtrsv
  tbsv  = C.dtbsv
  tpsv  = C.dtpsv
  hemv  = C.dsymv
  hbmv  = C.dsbmv
  hpmv  = C.dspmv
  geru  = C.dger
  gerc  = C.dger
  her   = C.dsyr
  hpr   = C.dspr
  her2  = C.dsyr2
  hpr2  = C.dspr2
  gemm  = C.dgemm
  symm  = C.dsymm
  hemm  = C.dsymm
  syrk  = C.dsyrk
  herk  = C.dsyrk
  syr2k = C.dsyr2k
  her2k = C.dsyr2k
  trmm  = C.dtrmm
  trsm  = C.dtrsm

instance RealNumeric Double where
  rotg  = C.drotg
  rotmg = C.drotmg
  rot   = C.drot
  rotm  = C.drotm

instance Numeric (Complex Float) where
  type RealType (Complex Float) = Float
  dotu a b c d e = getReturnValue (C.cdotu_sub a b c d e)
  dotc a b c d e = getReturnValue (C.cdotc_sub a b c d e)
  nrm2  = C.scnrm2
  asum  = C.scasum
  iamax = C.icamax
  swap  = C.cswap
  copy  = C.ccopy
  axpy  = C.caxpy
  scal n (alpha :+ 0) = C.csscal n alpha
  scal n  alpha       = C.cscal  n alpha
  gemv  = C.cgemv
  gbmv  = C.cgbmv
  trmv  = C.ctrmv
  tbmv  = C.ctbmv
  tpmv  = C.ctpmv
  trsv  = C.ctrsv
  tbsv  = C.ctbsv
  tpsv  = C.ctpsv
  hemv  = C.chemv
  hbmv  = C.chbmv
  hpmv  = C.chpmv
  geru  = C.cgeru
  gerc  = C.cgerc
  her   = C.cher
  hpr   = C.chpr
  her2  = C.cher2
  hpr2  = C.chpr2
  gemm  = C.cgemm
  symm  = C.csymm
  hemm  = C.chemm
  syrk  = C.csyrk
  herk  = C.cherk
  syr2k = C.csyr2k
  her2k = C.cher2k
  trmm  = C.ctrmm
  trsm  = C.ctrsm

instance Numeric (Complex Double) where
  type RealType (Complex Double) = Double
  dotu a b c d e = getReturnValue (C.zdotu_sub a b c d e)
  dotc a b c d e = getReturnValue (C.zdotc_sub a b c d e)
  nrm2  = C.dznrm2
  asum  = C.dzasum
  iamax = C.izamax
  swap  = C.zswap
  copy  = C.zcopy
  axpy  = C.zaxpy
  scal n (alpha :+ 0) = C.zdscal n alpha
  scal n  alpha       = C.zscal  n alpha
  gemv  = C.zgemv
  gbmv  = C.zgbmv
  trmv  = C.ztrmv
  tbmv  = C.ztbmv
  tpmv  = C.ztpmv
  trsv  = C.ztrsv
  tbsv  = C.ztbsv
  tpsv  = C.ztpsv
  hemv  = C.zhemv
  hbmv  = C.zhbmv
  hpmv  = C.zhpmv
  geru  = C.zgeru
  gerc  = C.zgerc
  her   = C.zher
  hpr   = C.zhpr
  her2  = C.zher2
  hpr2  = C.zhpr2
  gemm  = C.zgemm
  symm  = C.zsymm
  hemm  = C.zhemm
  syrk  = C.zsyrk
  herk  = C.zherk
  syr2k = C.zsyr2k
  her2k = C.zher2k
  trmm  = C.ztrmm
  trsm  = C.ztrsm
