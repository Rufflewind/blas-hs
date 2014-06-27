{-# LANGUAGE ForeignFunctionInterface #-}
{-|
Module: Blas.Primitive.Safe
Copyright: (c) 2014 Phil Ruffwind
License: MIT
Maintainer: rf@rufflewind.com
Stability: experimental
Foreign function interface to Blas.  These functions use safe foreign calls, which do not block other Haskell threads.
-}module Blas.Primitive.Safe (sdsdot, dsdot, sdot, ddot, cdotu_sub, zdotu_sub, cdotc_sub, zdotc_sub, snrm2, dnrm2, scnrm2, dznrm2, sasum, dasum, scasum, dzasum, isamax, idamax, icamax, izamax, sswap, dswap, cswap, zswap, scopy, dcopy, ccopy, zcopy, saxpy, daxpy, caxpy, zaxpy, srotg, drotg, srotmg, drotmg, srot, drot, srotm, drotm, sscal, dscal, cscal, csscal, zscal, zdscal, sgemv, dgemv, cgemv, zgemv, sgbmv, dgbmv, cgbmv, zgbmv, strmv, dtrmv, ctrmv, ztrmv, stbmv, dtbmv, ctbmv, ztbmv, stpmv, dtpmv, ctpmv, ztpmv, strsv, dtrsv, ctrsv, ztrsv, stbsv, dtbsv, ctbsv, ztbsv, stpsv, dtpsv, ctpsv, ztpsv, ssymv, dsymv, ssbmv, dsbmv, sspmv, dspmv, sger, dger, ssyr, dsyr, ssyr2, dsyr2, sspr, dspr, sspr2, dspr2, chemv, zhemv, chbmv, zhbmv, chpmv, zhpmv, cgeru, zgeru, cgerc, zgerc, cher, zher, chpr, zhpr, cher2, zher2, chpr2, zhpr2, sgemm, dgemm, cgemm, zgemm, ssymm, dsymm, csymm, zsymm, ssyrk, dsyrk, csyrk, zsyrk, ssyr2k, dsyr2k, csyr2k, zsyr2k, chemm, zhemm, cherk, zherk, cher2k, zher2k, strmm, dtrmm, ctrmm, ztrmm, strsm, dtrsm, ctrsm, ztrsm) where
import Prelude hiding (Either(Left, Right))
import Foreign.Storable.Complex ()
import Data.Complex (Complex)
import FFI
import Blas.Primitive.Types
#include <cblas.h>
{# fun  cblas_sdsdot as sdsdot {  `Int' , CFloat `Float' , castPtr `Ptr Float' ,  `Int' , castPtr `Ptr Float' ,  `Int' } -> `Float' fromCFloat #}

{# fun  cblas_dsdot as dsdot {  `Int' , castPtr `Ptr Float' ,  `Int' , castPtr `Ptr Float' ,  `Int' } -> `Double' fromCDouble #}

{# fun  cblas_sdot as sdot {  `Int' , castPtr `Ptr Float' ,  `Int' , castPtr `Ptr Float' ,  `Int' } -> `Float' fromCFloat #}

{# fun  cblas_ddot as ddot {  `Int' , castPtr `Ptr Double' ,  `Int' , castPtr `Ptr Double' ,  `Int' } -> `Double' fromCDouble #}

{# fun  cblas_cdotu_sub as cdotu_sub {  `Int' , castPtr `Ptr (Complex Float)' ,  `Int' , castPtr `Ptr (Complex Float)' ,  `Int' , castPtr `Ptr (Complex Float)' } -> `()'  #}

{# fun  cblas_zdotu_sub as zdotu_sub {  `Int' , castPtr `Ptr (Complex Double)' ,  `Int' , castPtr `Ptr (Complex Double)' ,  `Int' , castPtr `Ptr (Complex Double)' } -> `()'  #}

{# fun  cblas_cdotc_sub as cdotc_sub {  `Int' , castPtr `Ptr (Complex Float)' ,  `Int' , castPtr `Ptr (Complex Float)' ,  `Int' , castPtr `Ptr (Complex Float)' } -> `()'  #}

{# fun  cblas_zdotc_sub as zdotc_sub {  `Int' , castPtr `Ptr (Complex Double)' ,  `Int' , castPtr `Ptr (Complex Double)' ,  `Int' , castPtr `Ptr (Complex Double)' } -> `()'  #}

{# fun  cblas_snrm2 as snrm2 {  `Int' , castPtr `Ptr Float' ,  `Int' } -> `Float' fromCFloat #}

{# fun  cblas_dnrm2 as dnrm2 {  `Int' , castPtr `Ptr Double' ,  `Int' } -> `Double' fromCDouble #}

{# fun  cblas_scnrm2 as scnrm2 {  `Int' , castPtr `Ptr (Complex Float)' ,  `Int' } -> `Float' fromCFloat #}

{# fun  cblas_dznrm2 as dznrm2 {  `Int' , castPtr `Ptr (Complex Double)' ,  `Int' } -> `Double' fromCDouble #}

{# fun  cblas_sasum as sasum {  `Int' , castPtr `Ptr Float' ,  `Int' } -> `Float' fromCFloat #}

{# fun  cblas_dasum as dasum {  `Int' , castPtr `Ptr Double' ,  `Int' } -> `Double' fromCDouble #}

{# fun  cblas_scasum as scasum {  `Int' , castPtr `Ptr (Complex Float)' ,  `Int' } -> `Float' fromCFloat #}

{# fun  cblas_dzasum as dzasum {  `Int' , castPtr `Ptr (Complex Double)' ,  `Int' } -> `Double' fromCDouble #}

{# fun  cblas_isamax as isamax {  `Int' , castPtr `Ptr Float' ,  `Int' } -> `Int'  #}

{# fun  cblas_idamax as idamax {  `Int' , castPtr `Ptr Double' ,  `Int' } -> `Int'  #}

{# fun  cblas_icamax as icamax {  `Int' , castPtr `Ptr (Complex Float)' ,  `Int' } -> `Int'  #}

{# fun  cblas_izamax as izamax {  `Int' , castPtr `Ptr (Complex Double)' ,  `Int' } -> `Int'  #}

{# fun  cblas_sswap as sswap {  `Int' , castPtr `Ptr Float' ,  `Int' , castPtr `Ptr Float' ,  `Int' } -> `()'  #}

{# fun  cblas_dswap as dswap {  `Int' , castPtr `Ptr Double' ,  `Int' , castPtr `Ptr Double' ,  `Int' } -> `()'  #}

{# fun  cblas_cswap as cswap {  `Int' , castPtr `Ptr (Complex Float)' ,  `Int' , castPtr `Ptr (Complex Float)' ,  `Int' } -> `()'  #}

{# fun  cblas_zswap as zswap {  `Int' , castPtr `Ptr (Complex Double)' ,  `Int' , castPtr `Ptr (Complex Double)' ,  `Int' } -> `()'  #}

{# fun  cblas_scopy as scopy {  `Int' , castPtr `Ptr Float' ,  `Int' , castPtr `Ptr Float' ,  `Int' } -> `()'  #}

{# fun  cblas_dcopy as dcopy {  `Int' , castPtr `Ptr Double' ,  `Int' , castPtr `Ptr Double' ,  `Int' } -> `()'  #}

{# fun  cblas_ccopy as ccopy {  `Int' , castPtr `Ptr (Complex Float)' ,  `Int' , castPtr `Ptr (Complex Float)' ,  `Int' } -> `()'  #}

{# fun  cblas_zcopy as zcopy {  `Int' , castPtr `Ptr (Complex Double)' ,  `Int' , castPtr `Ptr (Complex Double)' ,  `Int' } -> `()'  #}

{# fun  cblas_saxpy as saxpy {  `Int' , CFloat `Float' , castPtr `Ptr Float' ,  `Int' , castPtr `Ptr Float' ,  `Int' } -> `()'  #}

{# fun  cblas_daxpy as daxpy {  `Int' , CDouble `Double' , castPtr `Ptr Double' ,  `Int' , castPtr `Ptr Double' ,  `Int' } -> `()'  #}

{# fun  cblas_caxpy as caxpy {  `Int' , withVoidPtr* `Complex Float' , castPtr `Ptr (Complex Float)' ,  `Int' , castPtr `Ptr (Complex Float)' ,  `Int' } -> `()'  #}

{# fun  cblas_zaxpy as zaxpy {  `Int' , withVoidPtr* `Complex Double' , castPtr `Ptr (Complex Double)' ,  `Int' , castPtr `Ptr (Complex Double)' ,  `Int' } -> `()'  #}

{# fun  cblas_srotg as srotg { castPtr `Ptr Float' , castPtr `Ptr Float' , castPtr `Ptr Float' , castPtr `Ptr Float' } -> `()'  #}

{# fun  cblas_drotg as drotg { castPtr `Ptr Double' , castPtr `Ptr Double' , castPtr `Ptr Double' , castPtr `Ptr Double' } -> `()'  #}

{# fun  cblas_srotmg as srotmg { castPtr `Ptr Float' , castPtr `Ptr Float' , castPtr `Ptr Float' , CFloat `Float' , castPtr `Ptr Float' } -> `()'  #}

{# fun  cblas_drotmg as drotmg { castPtr `Ptr Double' , castPtr `Ptr Double' , castPtr `Ptr Double' , CDouble `Double' , castPtr `Ptr Double' } -> `()'  #}

{# fun  cblas_srot as srot {  `Int' , castPtr `Ptr Float' ,  `Int' , castPtr `Ptr Float' ,  `Int' , CFloat `Float' , CFloat `Float' } -> `()'  #}

{# fun  cblas_drot as drot {  `Int' , castPtr `Ptr Double' ,  `Int' , castPtr `Ptr Double' ,  `Int' , CDouble `Double' , CDouble `Double' } -> `()'  #}

{# fun  cblas_srotm as srotm {  `Int' , castPtr `Ptr Float' ,  `Int' , castPtr `Ptr Float' ,  `Int' , castPtr `Ptr Float' } -> `()'  #}

{# fun  cblas_drotm as drotm {  `Int' , castPtr `Ptr Double' ,  `Int' , castPtr `Ptr Double' ,  `Int' , castPtr `Ptr Double' } -> `()'  #}

{# fun  cblas_sscal as sscal {  `Int' , CFloat `Float' , castPtr `Ptr Float' ,  `Int' } -> `()'  #}

{# fun  cblas_dscal as dscal {  `Int' , CDouble `Double' , castPtr `Ptr Double' ,  `Int' } -> `()'  #}

{# fun  cblas_cscal as cscal {  `Int' , withVoidPtr* `Complex Float' , castPtr `Ptr (Complex Float)' ,  `Int' } -> `()'  #}

{# fun  cblas_csscal as csscal {  `Int' , CFloat `Float' , castPtr `Ptr (Complex Float)' ,  `Int' } -> `()'  #}

{# fun  cblas_zscal as zscal {  `Int' , withVoidPtr* `Complex Double' , castPtr `Ptr (Complex Double)' ,  `Int' } -> `()'  #}

{# fun  cblas_zdscal as zdscal {  `Int' , CDouble `Double' , castPtr `Ptr (Complex Double)' ,  `Int' } -> `()'  #}

{# fun  cblas_sgemv as sgemv { toCEnum `Order' , toCEnum `Transpose' ,  `Int' ,  `Int' , CFloat `Float' , castPtr `Ptr Float' ,  `Int' , castPtr `Ptr Float' ,  `Int' , CFloat `Float' , castPtr `Ptr Float' ,  `Int' } -> `()'  #}

{# fun  cblas_dgemv as dgemv { toCEnum `Order' , toCEnum `Transpose' ,  `Int' ,  `Int' , CDouble `Double' , castPtr `Ptr Double' ,  `Int' , castPtr `Ptr Double' ,  `Int' , CDouble `Double' , castPtr `Ptr Double' ,  `Int' } -> `()'  #}

{# fun  cblas_cgemv as cgemv { toCEnum `Order' , toCEnum `Transpose' ,  `Int' ,  `Int' , withVoidPtr* `Complex Float' , castPtr `Ptr (Complex Float)' ,  `Int' , castPtr `Ptr (Complex Float)' ,  `Int' , withVoidPtr* `Complex Float' , castPtr `Ptr (Complex Float)' ,  `Int' } -> `()'  #}

{# fun  cblas_zgemv as zgemv { toCEnum `Order' , toCEnum `Transpose' ,  `Int' ,  `Int' , withVoidPtr* `Complex Double' , castPtr `Ptr (Complex Double)' ,  `Int' , castPtr `Ptr (Complex Double)' ,  `Int' , withVoidPtr* `Complex Double' , castPtr `Ptr (Complex Double)' ,  `Int' } -> `()'  #}

{# fun  cblas_sgbmv as sgbmv { toCEnum `Order' , toCEnum `Transpose' ,  `Int' ,  `Int' ,  `Int' ,  `Int' , CFloat `Float' , castPtr `Ptr Float' ,  `Int' , castPtr `Ptr Float' ,  `Int' , CFloat `Float' , castPtr `Ptr Float' ,  `Int' } -> `()'  #}

{# fun  cblas_dgbmv as dgbmv { toCEnum `Order' , toCEnum `Transpose' ,  `Int' ,  `Int' ,  `Int' ,  `Int' , CDouble `Double' , castPtr `Ptr Double' ,  `Int' , castPtr `Ptr Double' ,  `Int' , CDouble `Double' , castPtr `Ptr Double' ,  `Int' } -> `()'  #}

{# fun  cblas_cgbmv as cgbmv { toCEnum `Order' , toCEnum `Transpose' ,  `Int' ,  `Int' ,  `Int' ,  `Int' , withVoidPtr* `Complex Float' , castPtr `Ptr (Complex Float)' ,  `Int' , castPtr `Ptr (Complex Float)' ,  `Int' , withVoidPtr* `Complex Float' , castPtr `Ptr (Complex Float)' ,  `Int' } -> `()'  #}

{# fun  cblas_zgbmv as zgbmv { toCEnum `Order' , toCEnum `Transpose' ,  `Int' ,  `Int' ,  `Int' ,  `Int' , withVoidPtr* `Complex Double' , castPtr `Ptr (Complex Double)' ,  `Int' , castPtr `Ptr (Complex Double)' ,  `Int' , withVoidPtr* `Complex Double' , castPtr `Ptr (Complex Double)' ,  `Int' } -> `()'  #}

{# fun  cblas_strmv as strmv { toCEnum `Order' , toCEnum `Uplo' , toCEnum `Transpose' , toCEnum `Diag' ,  `Int' , castPtr `Ptr Float' ,  `Int' , castPtr `Ptr Float' ,  `Int' } -> `()'  #}

{# fun  cblas_dtrmv as dtrmv { toCEnum `Order' , toCEnum `Uplo' , toCEnum `Transpose' , toCEnum `Diag' ,  `Int' , castPtr `Ptr Double' ,  `Int' , castPtr `Ptr Double' ,  `Int' } -> `()'  #}

{# fun  cblas_ctrmv as ctrmv { toCEnum `Order' , toCEnum `Uplo' , toCEnum `Transpose' , toCEnum `Diag' ,  `Int' , castPtr `Ptr (Complex Float)' ,  `Int' , castPtr `Ptr (Complex Float)' ,  `Int' } -> `()'  #}

{# fun  cblas_ztrmv as ztrmv { toCEnum `Order' , toCEnum `Uplo' , toCEnum `Transpose' , toCEnum `Diag' ,  `Int' , castPtr `Ptr (Complex Double)' ,  `Int' , castPtr `Ptr (Complex Double)' ,  `Int' } -> `()'  #}

{# fun  cblas_stbmv as stbmv { toCEnum `Order' , toCEnum `Uplo' , toCEnum `Transpose' , toCEnum `Diag' ,  `Int' ,  `Int' , castPtr `Ptr Float' ,  `Int' , castPtr `Ptr Float' ,  `Int' } -> `()'  #}

{# fun  cblas_dtbmv as dtbmv { toCEnum `Order' , toCEnum `Uplo' , toCEnum `Transpose' , toCEnum `Diag' ,  `Int' ,  `Int' , castPtr `Ptr Double' ,  `Int' , castPtr `Ptr Double' ,  `Int' } -> `()'  #}

{# fun  cblas_ctbmv as ctbmv { toCEnum `Order' , toCEnum `Uplo' , toCEnum `Transpose' , toCEnum `Diag' ,  `Int' ,  `Int' , castPtr `Ptr (Complex Float)' ,  `Int' , castPtr `Ptr (Complex Float)' ,  `Int' } -> `()'  #}

{# fun  cblas_ztbmv as ztbmv { toCEnum `Order' , toCEnum `Uplo' , toCEnum `Transpose' , toCEnum `Diag' ,  `Int' ,  `Int' , castPtr `Ptr (Complex Double)' ,  `Int' , castPtr `Ptr (Complex Double)' ,  `Int' } -> `()'  #}

{# fun  cblas_stpmv as stpmv { toCEnum `Order' , toCEnum `Uplo' , toCEnum `Transpose' , toCEnum `Diag' ,  `Int' , castPtr `Ptr Float' , castPtr `Ptr Float' ,  `Int' } -> `()'  #}

{# fun  cblas_dtpmv as dtpmv { toCEnum `Order' , toCEnum `Uplo' , toCEnum `Transpose' , toCEnum `Diag' ,  `Int' , castPtr `Ptr Double' , castPtr `Ptr Double' ,  `Int' } -> `()'  #}

{# fun  cblas_ctpmv as ctpmv { toCEnum `Order' , toCEnum `Uplo' , toCEnum `Transpose' , toCEnum `Diag' ,  `Int' , castPtr `Ptr (Complex Float)' , castPtr `Ptr (Complex Float)' ,  `Int' } -> `()'  #}

{# fun  cblas_ztpmv as ztpmv { toCEnum `Order' , toCEnum `Uplo' , toCEnum `Transpose' , toCEnum `Diag' ,  `Int' , castPtr `Ptr (Complex Double)' , castPtr `Ptr (Complex Double)' ,  `Int' } -> `()'  #}

{# fun  cblas_strsv as strsv { toCEnum `Order' , toCEnum `Uplo' , toCEnum `Transpose' , toCEnum `Diag' ,  `Int' , castPtr `Ptr Float' ,  `Int' , castPtr `Ptr Float' ,  `Int' } -> `()'  #}

{# fun  cblas_dtrsv as dtrsv { toCEnum `Order' , toCEnum `Uplo' , toCEnum `Transpose' , toCEnum `Diag' ,  `Int' , castPtr `Ptr Double' ,  `Int' , castPtr `Ptr Double' ,  `Int' } -> `()'  #}

{# fun  cblas_ctrsv as ctrsv { toCEnum `Order' , toCEnum `Uplo' , toCEnum `Transpose' , toCEnum `Diag' ,  `Int' , castPtr `Ptr (Complex Float)' ,  `Int' , castPtr `Ptr (Complex Float)' ,  `Int' } -> `()'  #}

{# fun  cblas_ztrsv as ztrsv { toCEnum `Order' , toCEnum `Uplo' , toCEnum `Transpose' , toCEnum `Diag' ,  `Int' , castPtr `Ptr (Complex Double)' ,  `Int' , castPtr `Ptr (Complex Double)' ,  `Int' } -> `()'  #}

{# fun  cblas_stbsv as stbsv { toCEnum `Order' , toCEnum `Uplo' , toCEnum `Transpose' , toCEnum `Diag' ,  `Int' ,  `Int' , castPtr `Ptr Float' ,  `Int' , castPtr `Ptr Float' ,  `Int' } -> `()'  #}

{# fun  cblas_dtbsv as dtbsv { toCEnum `Order' , toCEnum `Uplo' , toCEnum `Transpose' , toCEnum `Diag' ,  `Int' ,  `Int' , castPtr `Ptr Double' ,  `Int' , castPtr `Ptr Double' ,  `Int' } -> `()'  #}

{# fun  cblas_ctbsv as ctbsv { toCEnum `Order' , toCEnum `Uplo' , toCEnum `Transpose' , toCEnum `Diag' ,  `Int' ,  `Int' , castPtr `Ptr (Complex Float)' ,  `Int' , castPtr `Ptr (Complex Float)' ,  `Int' } -> `()'  #}

{# fun  cblas_ztbsv as ztbsv { toCEnum `Order' , toCEnum `Uplo' , toCEnum `Transpose' , toCEnum `Diag' ,  `Int' ,  `Int' , castPtr `Ptr (Complex Double)' ,  `Int' , castPtr `Ptr (Complex Double)' ,  `Int' } -> `()'  #}

{# fun  cblas_stpsv as stpsv { toCEnum `Order' , toCEnum `Uplo' , toCEnum `Transpose' , toCEnum `Diag' ,  `Int' , castPtr `Ptr Float' , castPtr `Ptr Float' ,  `Int' } -> `()'  #}

{# fun  cblas_dtpsv as dtpsv { toCEnum `Order' , toCEnum `Uplo' , toCEnum `Transpose' , toCEnum `Diag' ,  `Int' , castPtr `Ptr Double' , castPtr `Ptr Double' ,  `Int' } -> `()'  #}

{# fun  cblas_ctpsv as ctpsv { toCEnum `Order' , toCEnum `Uplo' , toCEnum `Transpose' , toCEnum `Diag' ,  `Int' , castPtr `Ptr (Complex Float)' , castPtr `Ptr (Complex Float)' ,  `Int' } -> `()'  #}

{# fun  cblas_ztpsv as ztpsv { toCEnum `Order' , toCEnum `Uplo' , toCEnum `Transpose' , toCEnum `Diag' ,  `Int' , castPtr `Ptr (Complex Double)' , castPtr `Ptr (Complex Double)' ,  `Int' } -> `()'  #}

{# fun  cblas_ssymv as ssymv { toCEnum `Order' , toCEnum `Uplo' ,  `Int' , CFloat `Float' , castPtr `Ptr Float' ,  `Int' , castPtr `Ptr Float' ,  `Int' , CFloat `Float' , castPtr `Ptr Float' ,  `Int' } -> `()'  #}

{# fun  cblas_dsymv as dsymv { toCEnum `Order' , toCEnum `Uplo' ,  `Int' , CDouble `Double' , castPtr `Ptr Double' ,  `Int' , castPtr `Ptr Double' ,  `Int' , CDouble `Double' , castPtr `Ptr Double' ,  `Int' } -> `()'  #}

{# fun  cblas_ssbmv as ssbmv { toCEnum `Order' , toCEnum `Uplo' ,  `Int' ,  `Int' , CFloat `Float' , castPtr `Ptr Float' ,  `Int' , castPtr `Ptr Float' ,  `Int' , CFloat `Float' , castPtr `Ptr Float' ,  `Int' } -> `()'  #}

{# fun  cblas_dsbmv as dsbmv { toCEnum `Order' , toCEnum `Uplo' ,  `Int' ,  `Int' , CDouble `Double' , castPtr `Ptr Double' ,  `Int' , castPtr `Ptr Double' ,  `Int' , CDouble `Double' , castPtr `Ptr Double' ,  `Int' } -> `()'  #}

{# fun  cblas_sspmv as sspmv { toCEnum `Order' , toCEnum `Uplo' ,  `Int' , CFloat `Float' , castPtr `Ptr Float' , castPtr `Ptr Float' ,  `Int' , CFloat `Float' , castPtr `Ptr Float' ,  `Int' } -> `()'  #}

{# fun  cblas_dspmv as dspmv { toCEnum `Order' , toCEnum `Uplo' ,  `Int' , CDouble `Double' , castPtr `Ptr Double' , castPtr `Ptr Double' ,  `Int' , CDouble `Double' , castPtr `Ptr Double' ,  `Int' } -> `()'  #}

{# fun  cblas_sger as sger { toCEnum `Order' ,  `Int' ,  `Int' , CFloat `Float' , castPtr `Ptr Float' ,  `Int' , castPtr `Ptr Float' ,  `Int' , castPtr `Ptr Float' ,  `Int' } -> `()'  #}

{# fun  cblas_dger as dger { toCEnum `Order' ,  `Int' ,  `Int' , CDouble `Double' , castPtr `Ptr Double' ,  `Int' , castPtr `Ptr Double' ,  `Int' , castPtr `Ptr Double' ,  `Int' } -> `()'  #}

{# fun  cblas_ssyr as ssyr { toCEnum `Order' , toCEnum `Uplo' ,  `Int' , CFloat `Float' , castPtr `Ptr Float' ,  `Int' , castPtr `Ptr Float' ,  `Int' } -> `()'  #}

{# fun  cblas_dsyr as dsyr { toCEnum `Order' , toCEnum `Uplo' ,  `Int' , CDouble `Double' , castPtr `Ptr Double' ,  `Int' , castPtr `Ptr Double' ,  `Int' } -> `()'  #}

{# fun  cblas_ssyr2 as ssyr2 { toCEnum `Order' , toCEnum `Uplo' ,  `Int' , CFloat `Float' , castPtr `Ptr Float' ,  `Int' , castPtr `Ptr Float' ,  `Int' , castPtr `Ptr Float' ,  `Int' } -> `()'  #}

{# fun  cblas_dsyr2 as dsyr2 { toCEnum `Order' , toCEnum `Uplo' ,  `Int' , CDouble `Double' , castPtr `Ptr Double' ,  `Int' , castPtr `Ptr Double' ,  `Int' , castPtr `Ptr Double' ,  `Int' } -> `()'  #}

{# fun  cblas_sspr as sspr { toCEnum `Order' , toCEnum `Uplo' ,  `Int' , CFloat `Float' , castPtr `Ptr Float' ,  `Int' , castPtr `Ptr Float' } -> `()'  #}

{# fun  cblas_dspr as dspr { toCEnum `Order' , toCEnum `Uplo' ,  `Int' , CDouble `Double' , castPtr `Ptr Double' ,  `Int' , castPtr `Ptr Double' } -> `()'  #}

{# fun  cblas_sspr2 as sspr2 { toCEnum `Order' , toCEnum `Uplo' ,  `Int' , CFloat `Float' , castPtr `Ptr Float' ,  `Int' , castPtr `Ptr Float' ,  `Int' , castPtr `Ptr Float' } -> `()'  #}

{# fun  cblas_dspr2 as dspr2 { toCEnum `Order' , toCEnum `Uplo' ,  `Int' , CDouble `Double' , castPtr `Ptr Double' ,  `Int' , castPtr `Ptr Double' ,  `Int' , castPtr `Ptr Double' } -> `()'  #}

{# fun  cblas_chemv as chemv { toCEnum `Order' , toCEnum `Uplo' ,  `Int' , withVoidPtr* `Complex Float' , castPtr `Ptr (Complex Float)' ,  `Int' , castPtr `Ptr (Complex Float)' ,  `Int' , withVoidPtr* `Complex Float' , castPtr `Ptr (Complex Float)' ,  `Int' } -> `()'  #}

{# fun  cblas_zhemv as zhemv { toCEnum `Order' , toCEnum `Uplo' ,  `Int' , withVoidPtr* `Complex Double' , castPtr `Ptr (Complex Double)' ,  `Int' , castPtr `Ptr (Complex Double)' ,  `Int' , withVoidPtr* `Complex Double' , castPtr `Ptr (Complex Double)' ,  `Int' } -> `()'  #}

{# fun  cblas_chbmv as chbmv { toCEnum `Order' , toCEnum `Uplo' ,  `Int' ,  `Int' , withVoidPtr* `Complex Float' , castPtr `Ptr (Complex Float)' ,  `Int' , castPtr `Ptr (Complex Float)' ,  `Int' , withVoidPtr* `Complex Float' , castPtr `Ptr (Complex Float)' ,  `Int' } -> `()'  #}

{# fun  cblas_zhbmv as zhbmv { toCEnum `Order' , toCEnum `Uplo' ,  `Int' ,  `Int' , withVoidPtr* `Complex Double' , castPtr `Ptr (Complex Double)' ,  `Int' , castPtr `Ptr (Complex Double)' ,  `Int' , withVoidPtr* `Complex Double' , castPtr `Ptr (Complex Double)' ,  `Int' } -> `()'  #}

{# fun  cblas_chpmv as chpmv { toCEnum `Order' , toCEnum `Uplo' ,  `Int' , withVoidPtr* `Complex Float' , castPtr `Ptr (Complex Float)' , castPtr `Ptr (Complex Float)' ,  `Int' , withVoidPtr* `Complex Float' , castPtr `Ptr (Complex Float)' ,  `Int' } -> `()'  #}

{# fun  cblas_zhpmv as zhpmv { toCEnum `Order' , toCEnum `Uplo' ,  `Int' , withVoidPtr* `Complex Double' , castPtr `Ptr (Complex Double)' , castPtr `Ptr (Complex Double)' ,  `Int' , withVoidPtr* `Complex Double' , castPtr `Ptr (Complex Double)' ,  `Int' } -> `()'  #}

{# fun  cblas_cgeru as cgeru { toCEnum `Order' ,  `Int' ,  `Int' , withVoidPtr* `Complex Float' , castPtr `Ptr (Complex Float)' ,  `Int' , castPtr `Ptr (Complex Float)' ,  `Int' , castPtr `Ptr (Complex Float)' ,  `Int' } -> `()'  #}

{# fun  cblas_zgeru as zgeru { toCEnum `Order' ,  `Int' ,  `Int' , withVoidPtr* `Complex Double' , castPtr `Ptr (Complex Double)' ,  `Int' , castPtr `Ptr (Complex Double)' ,  `Int' , castPtr `Ptr (Complex Double)' ,  `Int' } -> `()'  #}

{# fun  cblas_cgerc as cgerc { toCEnum `Order' ,  `Int' ,  `Int' , withVoidPtr* `Complex Float' , castPtr `Ptr (Complex Float)' ,  `Int' , castPtr `Ptr (Complex Float)' ,  `Int' , castPtr `Ptr (Complex Float)' ,  `Int' } -> `()'  #}

{# fun  cblas_zgerc as zgerc { toCEnum `Order' ,  `Int' ,  `Int' , withVoidPtr* `Complex Double' , castPtr `Ptr (Complex Double)' ,  `Int' , castPtr `Ptr (Complex Double)' ,  `Int' , castPtr `Ptr (Complex Double)' ,  `Int' } -> `()'  #}

{# fun  cblas_cher as cher { toCEnum `Order' , toCEnum `Uplo' ,  `Int' , CFloat `Float' , castPtr `Ptr (Complex Float)' ,  `Int' , castPtr `Ptr (Complex Float)' ,  `Int' } -> `()'  #}

{# fun  cblas_zher as zher { toCEnum `Order' , toCEnum `Uplo' ,  `Int' , CDouble `Double' , castPtr `Ptr (Complex Double)' ,  `Int' , castPtr `Ptr (Complex Double)' ,  `Int' } -> `()'  #}

{# fun  cblas_chpr as chpr { toCEnum `Order' , toCEnum `Uplo' ,  `Int' , CFloat `Float' , castPtr `Ptr (Complex Float)' ,  `Int' , castPtr `Ptr (Complex Float)' } -> `()'  #}

{# fun  cblas_zhpr as zhpr { toCEnum `Order' , toCEnum `Uplo' ,  `Int' , CDouble `Double' , castPtr `Ptr (Complex Double)' ,  `Int' , castPtr `Ptr (Complex Double)' } -> `()'  #}

{# fun  cblas_cher2 as cher2 { toCEnum `Order' , toCEnum `Uplo' ,  `Int' , withVoidPtr* `Complex Float' , castPtr `Ptr (Complex Float)' ,  `Int' , castPtr `Ptr (Complex Float)' ,  `Int' , castPtr `Ptr (Complex Float)' ,  `Int' } -> `()'  #}

{# fun  cblas_zher2 as zher2 { toCEnum `Order' , toCEnum `Uplo' ,  `Int' , withVoidPtr* `Complex Double' , castPtr `Ptr (Complex Double)' ,  `Int' , castPtr `Ptr (Complex Double)' ,  `Int' , castPtr `Ptr (Complex Double)' ,  `Int' } -> `()'  #}

{# fun  cblas_chpr2 as chpr2 { toCEnum `Order' , toCEnum `Uplo' ,  `Int' , withVoidPtr* `Complex Float' , castPtr `Ptr (Complex Float)' ,  `Int' , castPtr `Ptr (Complex Float)' ,  `Int' , castPtr `Ptr (Complex Float)' } -> `()'  #}

{# fun  cblas_zhpr2 as zhpr2 { toCEnum `Order' , toCEnum `Uplo' ,  `Int' , withVoidPtr* `Complex Double' , castPtr `Ptr (Complex Double)' ,  `Int' , castPtr `Ptr (Complex Double)' ,  `Int' , castPtr `Ptr (Complex Double)' } -> `()'  #}

{# fun  cblas_sgemm as sgemm { toCEnum `Order' , toCEnum `Transpose' , toCEnum `Transpose' ,  `Int' ,  `Int' ,  `Int' , CFloat `Float' , castPtr `Ptr Float' ,  `Int' , castPtr `Ptr Float' ,  `Int' , CFloat `Float' , castPtr `Ptr Float' ,  `Int' } -> `()'  #}

{# fun  cblas_dgemm as dgemm { toCEnum `Order' , toCEnum `Transpose' , toCEnum `Transpose' ,  `Int' ,  `Int' ,  `Int' , CDouble `Double' , castPtr `Ptr Double' ,  `Int' , castPtr `Ptr Double' ,  `Int' , CDouble `Double' , castPtr `Ptr Double' ,  `Int' } -> `()'  #}

{# fun  cblas_cgemm as cgemm { toCEnum `Order' , toCEnum `Transpose' , toCEnum `Transpose' ,  `Int' ,  `Int' ,  `Int' , withVoidPtr* `Complex Float' , castPtr `Ptr (Complex Float)' ,  `Int' , castPtr `Ptr (Complex Float)' ,  `Int' , withVoidPtr* `Complex Float' , castPtr `Ptr (Complex Float)' ,  `Int' } -> `()'  #}

{# fun  cblas_zgemm as zgemm { toCEnum `Order' , toCEnum `Transpose' , toCEnum `Transpose' ,  `Int' ,  `Int' ,  `Int' , withVoidPtr* `Complex Double' , castPtr `Ptr (Complex Double)' ,  `Int' , castPtr `Ptr (Complex Double)' ,  `Int' , withVoidPtr* `Complex Double' , castPtr `Ptr (Complex Double)' ,  `Int' } -> `()'  #}

{# fun  cblas_ssymm as ssymm { toCEnum `Order' , toCEnum `Side' , toCEnum `Uplo' ,  `Int' ,  `Int' , CFloat `Float' , castPtr `Ptr Float' ,  `Int' , castPtr `Ptr Float' ,  `Int' , CFloat `Float' , castPtr `Ptr Float' ,  `Int' } -> `()'  #}

{# fun  cblas_dsymm as dsymm { toCEnum `Order' , toCEnum `Side' , toCEnum `Uplo' ,  `Int' ,  `Int' , CDouble `Double' , castPtr `Ptr Double' ,  `Int' , castPtr `Ptr Double' ,  `Int' , CDouble `Double' , castPtr `Ptr Double' ,  `Int' } -> `()'  #}

{# fun  cblas_csymm as csymm { toCEnum `Order' , toCEnum `Side' , toCEnum `Uplo' ,  `Int' ,  `Int' , withVoidPtr* `Complex Float' , castPtr `Ptr (Complex Float)' ,  `Int' , castPtr `Ptr (Complex Float)' ,  `Int' , withVoidPtr* `Complex Float' , castPtr `Ptr (Complex Float)' ,  `Int' } -> `()'  #}

{# fun  cblas_zsymm as zsymm { toCEnum `Order' , toCEnum `Side' , toCEnum `Uplo' ,  `Int' ,  `Int' , withVoidPtr* `Complex Double' , castPtr `Ptr (Complex Double)' ,  `Int' , castPtr `Ptr (Complex Double)' ,  `Int' , withVoidPtr* `Complex Double' , castPtr `Ptr (Complex Double)' ,  `Int' } -> `()'  #}

{# fun  cblas_ssyrk as ssyrk { toCEnum `Order' , toCEnum `Uplo' , toCEnum `Transpose' ,  `Int' ,  `Int' , CFloat `Float' , castPtr `Ptr Float' ,  `Int' , CFloat `Float' , castPtr `Ptr Float' ,  `Int' } -> `()'  #}

{# fun  cblas_dsyrk as dsyrk { toCEnum `Order' , toCEnum `Uplo' , toCEnum `Transpose' ,  `Int' ,  `Int' , CDouble `Double' , castPtr `Ptr Double' ,  `Int' , CDouble `Double' , castPtr `Ptr Double' ,  `Int' } -> `()'  #}

{# fun  cblas_csyrk as csyrk { toCEnum `Order' , toCEnum `Uplo' , toCEnum `Transpose' ,  `Int' ,  `Int' , withVoidPtr* `Complex Float' , castPtr `Ptr (Complex Float)' ,  `Int' , withVoidPtr* `Complex Float' , castPtr `Ptr (Complex Float)' ,  `Int' } -> `()'  #}

{# fun  cblas_zsyrk as zsyrk { toCEnum `Order' , toCEnum `Uplo' , toCEnum `Transpose' ,  `Int' ,  `Int' , withVoidPtr* `Complex Double' , castPtr `Ptr (Complex Double)' ,  `Int' , withVoidPtr* `Complex Double' , castPtr `Ptr (Complex Double)' ,  `Int' } -> `()'  #}

{# fun  cblas_ssyr2k as ssyr2k { toCEnum `Order' , toCEnum `Uplo' , toCEnum `Transpose' ,  `Int' ,  `Int' , CFloat `Float' , castPtr `Ptr Float' ,  `Int' , castPtr `Ptr Float' ,  `Int' , CFloat `Float' , castPtr `Ptr Float' ,  `Int' } -> `()'  #}

{# fun  cblas_dsyr2k as dsyr2k { toCEnum `Order' , toCEnum `Uplo' , toCEnum `Transpose' ,  `Int' ,  `Int' , CDouble `Double' , castPtr `Ptr Double' ,  `Int' , castPtr `Ptr Double' ,  `Int' , CDouble `Double' , castPtr `Ptr Double' ,  `Int' } -> `()'  #}

{# fun  cblas_csyr2k as csyr2k { toCEnum `Order' , toCEnum `Uplo' , toCEnum `Transpose' ,  `Int' ,  `Int' , withVoidPtr* `Complex Float' , castPtr `Ptr (Complex Float)' ,  `Int' , castPtr `Ptr (Complex Float)' ,  `Int' , withVoidPtr* `Complex Float' , castPtr `Ptr (Complex Float)' ,  `Int' } -> `()'  #}

{# fun  cblas_zsyr2k as zsyr2k { toCEnum `Order' , toCEnum `Uplo' , toCEnum `Transpose' ,  `Int' ,  `Int' , withVoidPtr* `Complex Double' , castPtr `Ptr (Complex Double)' ,  `Int' , castPtr `Ptr (Complex Double)' ,  `Int' , withVoidPtr* `Complex Double' , castPtr `Ptr (Complex Double)' ,  `Int' } -> `()'  #}

{# fun  cblas_chemm as chemm { toCEnum `Order' , toCEnum `Side' , toCEnum `Uplo' ,  `Int' ,  `Int' , withVoidPtr* `Complex Float' , castPtr `Ptr (Complex Float)' ,  `Int' , castPtr `Ptr (Complex Float)' ,  `Int' , withVoidPtr* `Complex Float' , castPtr `Ptr (Complex Float)' ,  `Int' } -> `()'  #}

{# fun  cblas_zhemm as zhemm { toCEnum `Order' , toCEnum `Side' , toCEnum `Uplo' ,  `Int' ,  `Int' , withVoidPtr* `Complex Double' , castPtr `Ptr (Complex Double)' ,  `Int' , castPtr `Ptr (Complex Double)' ,  `Int' , withVoidPtr* `Complex Double' , castPtr `Ptr (Complex Double)' ,  `Int' } -> `()'  #}

{# fun  cblas_cherk as cherk { toCEnum `Order' , toCEnum `Uplo' , toCEnum `Transpose' ,  `Int' ,  `Int' , CFloat `Float' , castPtr `Ptr (Complex Float)' ,  `Int' , CFloat `Float' , castPtr `Ptr (Complex Float)' ,  `Int' } -> `()'  #}

{# fun  cblas_zherk as zherk { toCEnum `Order' , toCEnum `Uplo' , toCEnum `Transpose' ,  `Int' ,  `Int' , CDouble `Double' , castPtr `Ptr (Complex Double)' ,  `Int' , CDouble `Double' , castPtr `Ptr (Complex Double)' ,  `Int' } -> `()'  #}

{# fun  cblas_cher2k as cher2k { toCEnum `Order' , toCEnum `Uplo' , toCEnum `Transpose' ,  `Int' ,  `Int' , withVoidPtr* `Complex Float' , castPtr `Ptr (Complex Float)' ,  `Int' , castPtr `Ptr (Complex Float)' ,  `Int' , CFloat `Float' , castPtr `Ptr (Complex Float)' ,  `Int' } -> `()'  #}

{# fun  cblas_zher2k as zher2k { toCEnum `Order' , toCEnum `Uplo' , toCEnum `Transpose' ,  `Int' ,  `Int' , withVoidPtr* `Complex Double' , castPtr `Ptr (Complex Double)' ,  `Int' , castPtr `Ptr (Complex Double)' ,  `Int' , CDouble `Double' , castPtr `Ptr (Complex Double)' ,  `Int' } -> `()'  #}

{# fun  cblas_strmm as strmm { toCEnum `Order' , toCEnum `Side' , toCEnum `Uplo' , toCEnum `Transpose' , toCEnum `Diag' ,  `Int' ,  `Int' , CFloat `Float' , castPtr `Ptr Float' ,  `Int' , castPtr `Ptr Float' ,  `Int' } -> `()'  #}

{# fun  cblas_dtrmm as dtrmm { toCEnum `Order' , toCEnum `Side' , toCEnum `Uplo' , toCEnum `Transpose' , toCEnum `Diag' ,  `Int' ,  `Int' , CDouble `Double' , castPtr `Ptr Double' ,  `Int' , castPtr `Ptr Double' ,  `Int' } -> `()'  #}

{# fun  cblas_ctrmm as ctrmm { toCEnum `Order' , toCEnum `Side' , toCEnum `Uplo' , toCEnum `Transpose' , toCEnum `Diag' ,  `Int' ,  `Int' , withVoidPtr* `Complex Float' , castPtr `Ptr (Complex Float)' ,  `Int' , castPtr `Ptr (Complex Float)' ,  `Int' } -> `()'  #}

{# fun  cblas_ztrmm as ztrmm { toCEnum `Order' , toCEnum `Side' , toCEnum `Uplo' , toCEnum `Transpose' , toCEnum `Diag' ,  `Int' ,  `Int' , withVoidPtr* `Complex Double' , castPtr `Ptr (Complex Double)' ,  `Int' , castPtr `Ptr (Complex Double)' ,  `Int' } -> `()'  #}

{# fun  cblas_strsm as strsm { toCEnum `Order' , toCEnum `Side' , toCEnum `Uplo' , toCEnum `Transpose' , toCEnum `Diag' ,  `Int' ,  `Int' , CFloat `Float' , castPtr `Ptr Float' ,  `Int' , castPtr `Ptr Float' ,  `Int' } -> `()'  #}

{# fun  cblas_dtrsm as dtrsm { toCEnum `Order' , toCEnum `Side' , toCEnum `Uplo' , toCEnum `Transpose' , toCEnum `Diag' ,  `Int' ,  `Int' , CDouble `Double' , castPtr `Ptr Double' ,  `Int' , castPtr `Ptr Double' ,  `Int' } -> `()'  #}

{# fun  cblas_ctrsm as ctrsm { toCEnum `Order' , toCEnum `Side' , toCEnum `Uplo' , toCEnum `Transpose' , toCEnum `Diag' ,  `Int' ,  `Int' , withVoidPtr* `Complex Float' , castPtr `Ptr (Complex Float)' ,  `Int' , castPtr `Ptr (Complex Float)' ,  `Int' } -> `()'  #}

{# fun  cblas_ztrsm as ztrsm { toCEnum `Order' , toCEnum `Side' , toCEnum `Uplo' , toCEnum `Transpose' , toCEnum `Diag' ,  `Int' ,  `Int' , withVoidPtr* `Complex Double' , castPtr `Ptr (Complex Double)' ,  `Int' , castPtr `Ptr (Complex Double)' ,  `Int' } -> `()'  #}
