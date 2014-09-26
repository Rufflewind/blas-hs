{- This file is auto-generated.  Do not edit directly. -}
{-# LANGUAGE FlexibleInstances, TypeFamilies #-}
{-|

Stability: experimental

Generic interface to Blas using unsafe foreign calls.  Refer to
the GHC documentation for more information regarding appropriate use of safe
and unsafe foreign calls.

The functions here are named in a similar fashion to the original Blas
interface, with the type-dependent letter(s) removed.  Some functions have
been merged with others to allow the interface to work on both real and
complex numbers.  If you can't a particular function, try looking for its
corresponding complex equivalent (e.g. @symv@ is a special case of 'hemv'
applied to real numbers).

It'd be really nice if these functions were actually documented.  Alas, for
the time being, you'll have to refer to the various Blas docs scattered across
the Internet.  Note that the interface is based off of /CBlas/, not Fortran
Blas, which is slightly different.

-}
module Blas.Generic.Unsafe
       ( Numeric(..)
       , RealNumeric(..)
       , D.dsdot
       , S.sdsdot
       ) where
import Data.Complex (Complex)
import Foreign (Ptr, Storable)
import Blas.Primitive.Types (Order, Transpose, Uplo, Diag, Side)
import qualified Blas.Specialized.Float.Unsafe as S
import qualified Blas.Specialized.Double.Unsafe as D
import qualified Blas.Specialized.ComplexFloat.Unsafe as C
import qualified Blas.Specialized.ComplexDouble.Unsafe as Z

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
        -> Int       -- ^ (@m@) Number of rows of @opa(a)@ and @c@.
        -> Int       -- ^ (@n@) Number of columns of @opb(b)@ and @c@.
        -> Int       -- ^ (@k@) Number of columns of @opa(a)@ and
                     --         number of of rows of @opb(b)@.
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


instance RealNumeric Float where
  rotg  = S.rotg
  rotmg = S.rotmg
  rot   = S.rot
  rotm  = S.rotm


instance Numeric Float where
  type RealType Float = Float
  dotu  = S.dotu
  dotc  = S.dotc
  nrm2  = S.nrm2
  asum  = S.asum
  iamax = S.iamax
  swap  = S.swap
  copy  = S.copy
  axpy  = S.axpy
  scal  = S.scal
  gemv  = S.gemv
  gbmv  = S.gbmv
  trmv  = S.trmv
  tbmv  = S.tbmv
  tpmv  = S.tpmv
  trsv  = S.trsv
  tbsv  = S.tbsv
  tpsv  = S.tpsv
  hemv  = S.hemv
  hbmv  = S.hbmv
  hpmv  = S.hpmv
  geru  = S.geru
  gerc  = S.gerc
  her   = S.her
  hpr   = S.hpr
  her2  = S.her2
  hpr2  = S.hpr2
  gemm  = S.gemm
  symm  = S.symm
  hemm  = S.hemm
  syrk  = S.syrk
  herk  = S.herk
  syr2k = S.syr2k
  her2k = S.her2k
  trmm  = S.trmm
  trsm  = S.trsm


instance RealNumeric Double where
  rotg  = D.rotg
  rotmg = D.rotmg
  rot   = D.rot
  rotm  = D.rotm


instance Numeric Double where
  type RealType Double = Double
  dotu  = D.dotu
  dotc  = D.dotc
  nrm2  = D.nrm2
  asum  = D.asum
  iamax = D.iamax
  swap  = D.swap
  copy  = D.copy
  axpy  = D.axpy
  scal  = D.scal
  gemv  = D.gemv
  gbmv  = D.gbmv
  trmv  = D.trmv
  tbmv  = D.tbmv
  tpmv  = D.tpmv
  trsv  = D.trsv
  tbsv  = D.tbsv
  tpsv  = D.tpsv
  hemv  = D.hemv
  hbmv  = D.hbmv
  hpmv  = D.hpmv
  geru  = D.geru
  gerc  = D.gerc
  her   = D.her
  hpr   = D.hpr
  her2  = D.her2
  hpr2  = D.hpr2
  gemm  = D.gemm
  symm  = D.symm
  hemm  = D.hemm
  syrk  = D.syrk
  herk  = D.herk
  syr2k = D.syr2k
  her2k = D.her2k
  trmm  = D.trmm
  trsm  = D.trsm




instance Numeric (Complex Float) where
  type RealType (Complex Float) = Float
  dotu  = C.dotu
  dotc  = C.dotc
  nrm2  = C.nrm2
  asum  = C.asum
  iamax = C.iamax
  swap  = C.swap
  copy  = C.copy
  axpy  = C.axpy
  scal  = C.scal
  gemv  = C.gemv
  gbmv  = C.gbmv
  trmv  = C.trmv
  tbmv  = C.tbmv
  tpmv  = C.tpmv
  trsv  = C.trsv
  tbsv  = C.tbsv
  tpsv  = C.tpsv
  hemv  = C.hemv
  hbmv  = C.hbmv
  hpmv  = C.hpmv
  geru  = C.geru
  gerc  = C.gerc
  her   = C.her
  hpr   = C.hpr
  her2  = C.her2
  hpr2  = C.hpr2
  gemm  = C.gemm
  symm  = C.symm
  hemm  = C.hemm
  syrk  = C.syrk
  herk  = C.herk
  syr2k = C.syr2k
  her2k = C.her2k
  trmm  = C.trmm
  trsm  = C.trsm




instance Numeric (Complex Double) where
  type RealType (Complex Double) = Double
  dotu  = Z.dotu
  dotc  = Z.dotc
  nrm2  = Z.nrm2
  asum  = Z.asum
  iamax = Z.iamax
  swap  = Z.swap
  copy  = Z.copy
  axpy  = Z.axpy
  scal  = Z.scal
  gemv  = Z.gemv
  gbmv  = Z.gbmv
  trmv  = Z.trmv
  tbmv  = Z.tbmv
  tpmv  = Z.tpmv
  trsv  = Z.trsv
  tbsv  = Z.tbsv
  tpsv  = Z.tpsv
  hemv  = Z.hemv
  hbmv  = Z.hbmv
  hpmv  = Z.hpmv
  geru  = Z.geru
  gerc  = Z.gerc
  her   = Z.her
  hpr   = Z.hpr
  her2  = Z.her2
  hpr2  = Z.hpr2
  gemm  = Z.gemm
  symm  = Z.symm
  hemm  = Z.hemm
  syrk  = Z.syrk
  herk  = Z.herk
  syr2k = Z.syr2k
  her2k = Z.her2k
  trmm  = Z.trmm
  trsm  = Z.trsm



