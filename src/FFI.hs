{-# OPTIONS_HADDOCK hide #-}
module FFI
       ( module FFI
       , module Foreign.C.Types
       , module Foreign
       ) where
import Foreign (Ptr, Storable, alloca, castPtr, peek, with)
import Foreign.C.Types

toCEnum :: (Enum a, Num b) => a -> b
toCEnum = fromIntegral . fromEnum

fromCEnum :: (Integral a, Enum b) => a -> b
fromCEnum = toEnum . fromIntegral

-- | Conversion by unwrapping the @newtype@.
fromCFloat :: CFloat -> Float
fromCFloat (CFloat x) = x

-- | Conversion by unwrapping the @newtype@.
fromCDouble :: CDouble -> Double
fromCDouble (CDouble x) = x

-- | Same as 'with' but accepts a @void@-pointer instead.
withVoidPtr :: Storable a => a -> (Ptr () -> IO c) -> IO c
withVoidPtr x f = with x (f . castPtr)

-- | Obtains a return value passed via a pointer argument.
getReturnValue :: Storable a => (Ptr a -> IO b) -> IO a
getReturnValue f = alloca $ \ x -> f x >> peek x
