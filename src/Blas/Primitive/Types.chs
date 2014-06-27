{-# LANGUAGE ForeignFunctionInterface #-}
{-|
Module: Blas.Primitive.Types
Copyright: (c) 2014 Phil Ruffwind
License: MIT
Maintainer: rf@rufflewind.com
Stability: experimental
Enumerations used by the Blas interface.
-}module Blas.Primitive.Types (Order(..), Transpose(..), Uplo(..), Diag(..), Side(..)) where
import Prelude hiding (Either(Left, Right))
import Foreign.Storable.Complex ()
import Data.Complex (Complex)
import FFI
#include <cblas.h>
{# enum CBLAS_ORDER as Order {} with prefix = "Cblas" deriving (Eq, Read, Show) #}

{# enum CBLAS_TRANSPOSE as Transpose {} with prefix = "Cblas" deriving (Eq, Read, Show) #}

{# enum CBLAS_UPLO as Uplo {} with prefix = "Cblas" deriving (Eq, Read, Show) #}

{# enum CBLAS_DIAG as Diag {} with prefix = "Cblas" deriving (Eq, Read, Show) #}

{# enum CBLAS_SIDE as Side {} with prefix = "Cblas" deriving (Eq, Read, Show) #}

