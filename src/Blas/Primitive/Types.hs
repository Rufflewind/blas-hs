{-|
Module: Blas.Primitive.Types
Copyright: (c) 2014 Phil Ruffwind
License: MIT
Maintainer: rf@rufflewind.com
Stability: experimental
Enumerations used by the Blas interface.
-}
module Blas.Primitive.Types
       ( Order(..)
       , Transpose(..)
       , Uplo(..)
       , Diag(..)
       , Side(..)
       ) where
import Prelude hiding (Either(Left, Right))

-- | Specifies the order of the matrix dimensions in memory.
data Order = RowMajor                   -- ^ Row-major order.
           | ColMajor                   -- ^ Column-major order.
           deriving (Eq, Read, Show)

instance Enum Order where
  fromEnum RowMajor = 101
  fromEnum ColMajor = 102
  toEnum 101 = RowMajor
  toEnum 102 = ColMajor
  toEnum unmatched = error ("Order.toEnum: Cannot match " ++ show unmatched)


-- | Specifies the unary operation @op@ for a matrix.
data Transpose = NoTrans                -- ^ No operation.
               | Trans                  -- ^ Transposition.
               | ConjTrans              -- ^ Conjugation and transposition.
               deriving (Eq, Read, Show)

instance Enum Transpose where
  fromEnum NoTrans   = 111
  fromEnum Trans     = 112
  fromEnum ConjTrans = 113
  toEnum 111 = NoTrans
  toEnum 112 = Trans
  toEnum 113 = ConjTrans
  toEnum unmatched =
    error $ "Transpose.toEnum: Cannot match " ++ show unmatched

data Uplo = Upper
          | Lower
          deriving (Eq, Read, Show)

instance Enum Uplo where
  fromEnum Upper = 121
  fromEnum Lower = 122
  toEnum 121 = Upper
  toEnum 122 = Lower
  toEnum unmatched =
    error $ "Uplo.toEnum: Cannot match " ++ show unmatched

data Diag = NonUnit
          | Unit
          deriving (Eq, Read, Show)

instance Enum Diag where
  fromEnum NonUnit = 131
  fromEnum Unit    = 132
  toEnum 131 = NonUnit
  toEnum 132 = Unit
  toEnum unmatched =
    error $ "Diag.toEnum: Cannot match " ++ show unmatched

data Side = Left
          | Right
          deriving (Eq, Read, Show)

instance Enum Side where
  fromEnum Left  = 141
  fromEnum Right = 142
  toEnum 141 = Left
  toEnum 142 = Right
  toEnum unmatched =
    error $ "Side.toEnum: Cannot match " ++ show unmatched
