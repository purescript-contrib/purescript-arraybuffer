module Data.ArrayBuffer.Typed( Writer()
                             , asInt8Array
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
                             ) where

import Prelude
import Data.ArrayBuffer.Types
import Data.ArrayBuffer.ArrayBuffer
import Data.Function
import Data.Maybe
import Control.Monad.Eff

foreign import data Writer :: !

-- | Create typed int8 array viewing the buffer mapped by the `DataView`
foreign import asInt8Array :: DataView -> Int8Array


-- | Create typed int16 array viewing the buffer mapped by the `DataView`
foreign import asInt16Array :: DataView -> Int16Array

-- | Create typed int32 array viewing the buffer mapped by the `DataView`
foreign import asInt32Array :: DataView -> Int32Array

-- | Create typed uint8 array viewing the buffer mapped by the `DataView`
foreign import asUint8Array :: DataView -> Uint8Array

-- | Create typed uint16 array viewing the buffer mapped by the `DataView`
foreign import asUint16Array :: DataView -> Uint16Array

-- | Create typed uint32 array viewing the buffer mapped by the `DataView`
foreign import asUint32Array :: DataView -> Uint32Array

-- | Create typed uint8 clamped array viewing the buffer mapped by the `DataView`
foreign import asUint8ClampedArray :: DataView -> Uint8ClampedArray

-- | Create typed float32 array viewing the buffer mapped by the `DataView`
foreign import asFloat32Array :: DataView -> Float32Array

-- | Create typed float64 array viewing the buffer mapped by the `DataView`
foreign import asFloat64Array :: DataView -> Float64Array

-- | Interpret typed array as a `DataView`.
foreign import dataView  :: forall a. ArrayView a -> DataView

foreign import setImpl :: forall a e. Fn3 (ArrayView a) ByteOffset (ArrayView a) (Eff (writer :: Writer | e) Unit)

-- | Stores multiple values in the last typed array, reading input values from ther first typed array.
set :: forall a e. ArrayView a -> ByteOffset -> ArrayView a -> Eff (writer :: Writer | e) Unit
set = runFn3 setImpl

foreign import unsafeAtImpl :: forall a. Fn2 (ArrayView a) Int Number

-- | Fetch element at index.
unsafeAt = runFn2 unsafeAtImpl

foreign import hasIndexImpl :: forall a. Fn2 (ArrayView a) Int Boolean

-- | Determine if a certain index is valid.
hasIndex = runFn2 hasIndexImpl

-- | Fetch element at index.
at :: forall a. ArrayView a -> Int -> Maybe Number
at a n = if a `hasIndex` n then
           Just $ unsafeAt a n
         else
           Nothing

-- | Turn typed array into an array.
foreign import toArray :: forall a. ArrayView a -> Array Number
