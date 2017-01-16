module Data.ArrayBuffer.Typed( asInt8Array
                             , asInt16Array
                             , asInt32Array
                             , asUint8Array
                             , asUint16Array
                             , asUint32Array
                             , asUint8ClampedArray
                             , asFloat32Array
                             , asFloat64Array
                             , dataView
                             , set
                             , unsafeAt
                             , hasIndex
                             , at
                             , toArray
                             , toIntArray
                             ) where

import Prelude
import Data.ArrayBuffer.Types (ArrayView, ByteOffset, DataView, Float64Array, Float32Array, Uint8ClampedArray, Uint32Array, Uint16Array, Uint8Array, Int32Array, Int16Array, Int8Array)
import Data.ArrayBuffer.ArrayBuffer(ARRAYBUFFER)
import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Data.Maybe (Maybe(..))
import Control.Monad.Eff (Eff)

-- | Create typed int8 array viewing the buffer mapped by the `DataView`
foreign import asInt8Array :: forall e. DataView -> Eff (arraybuffer :: ARRAYBUFFER | e) Int8Array

-- | Create typed int16 array viewing the buffer mapped by the `DataView`
foreign import asInt16Array :: forall e. DataView -> Eff (arraybuffer :: ARRAYBUFFER | e) Int16Array

-- | Create typed int32 array viewing the buffer mapped by the `DataView`
foreign import asInt32Array :: forall e. DataView -> Eff (arraybuffer :: ARRAYBUFFER | e) Int32Array

-- | Create typed uint8 array viewing the buffer mapped by the `DataView`
foreign import asUint8Array :: forall e. DataView -> Eff (arraybuffer :: ARRAYBUFFER | e) Uint8Array

-- | Create typed uint16 array viewing the buffer mapped by the `DataView`
foreign import asUint16Array :: forall e. DataView -> Eff (arraybuffer :: ARRAYBUFFER | e) Uint16Array

-- | Create typed uint32 array viewing the buffer mapped by the `DataView`
foreign import asUint32Array :: forall e. DataView -> Eff (arraybuffer :: ARRAYBUFFER | e) Uint32Array

-- | Create typed uint8 clamped array viewing the buffer mapped by the `DataView`
foreign import asUint8ClampedArray :: forall e. DataView -> Eff (arraybuffer :: ARRAYBUFFER | e) Uint8ClampedArray

-- | Create typed float32 array viewing the buffer mapped by the `DataView`
foreign import asFloat32Array :: forall e. DataView -> Eff (arraybuffer :: ARRAYBUFFER | e) Float32Array

-- | Create typed float64 array viewing the buffer mapped by the `DataView`
foreign import asFloat64Array :: forall e. DataView -> Eff (arraybuffer :: ARRAYBUFFER | e) Float64Array

-- | Interpret typed array as a `DataView`.
foreign import dataView  :: forall a. ArrayView a -> DataView

foreign import setImpl :: forall a e. Fn3 (ArrayView a) ByteOffset (ArrayView a) (Eff (arraybuffer :: ARRAYBUFFER | e) Unit)

-- | Stores multiple values in the last typed array, reading input values from ther first typed array.
set :: forall a e. ArrayView a -> ByteOffset -> ArrayView a -> Eff (arraybuffer :: ARRAYBUFFER | e) Unit
set = runFn3 setImpl

foreign import unsafeAtImpl :: forall a e. Fn2 (ArrayView a) Int (Eff (arraybuffer :: ARRAYBUFFER | e) Number)

-- | Fetch element at index.
unsafeAt :: forall a e. ArrayView a -> Int -> Eff (arraybuffer :: ARRAYBUFFER | e) Number
unsafeAt = runFn2 unsafeAtImpl

foreign import hasIndexImpl :: forall a. Fn2 (ArrayView a) Int Boolean

-- | Determine if a certain index is valid.
hasIndex :: forall a. ArrayView a -> Int -> Boolean
hasIndex = runFn2 hasIndexImpl

-- | Fetch element at index.
at :: forall a e. ArrayView a -> Int -> Eff (arraybuffer :: ARRAYBUFFER | e) (Maybe Number)
at a n = if a `hasIndex` n 
         then do
           x <- unsafeAt a n
           pure $ Just x
         else
           pure $ Nothing

-- | Turn typed array into an array.
foreign import toArray :: forall a e. ArrayView a -> Eff (arraybuffer :: ARRAYBUFFER | e) (Array Number)

-- | Turn typed array into integer array.
foreign import toIntArray :: forall a e. ArrayView a -> Eff (arraybuffer :: ARRAYBUFFER | e) (Array Int)

