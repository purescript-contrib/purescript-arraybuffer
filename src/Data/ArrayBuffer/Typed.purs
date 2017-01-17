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

import Control.Monad.Eff (Eff)
import Data.ArrayBuffer.ArrayBuffer (ARRAYBUFFER)
import Data.ArrayBuffer.Types (ArrayView, ByteOffset, DataView, Float64Array, Float32Array, Uint8ClampedArray, Uint32Array, Uint16Array, Uint8Array, Int32Array, Int16Array, Int8Array)
import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Data.Maybe (Maybe(..))
import Prelude (Unit, ($), bind, pure)

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
foreign import dataView  :: forall a e. ArrayView a -> Eff (arrayBuffer :: ARRAYBUFFER | e) DataView

foreign import setImpl :: forall a e. Fn3 (ArrayView a) ByteOffset (ArrayView a) (Eff (arrayBuffer :: ARRAYBUFFER | e) Unit)

-- | Stores multiple values in the last typed array, reading input values from ther first typed array.
set :: forall a e. ArrayView a -> ByteOffset -> ArrayView a -> Eff (arrayBuffer :: ARRAYBUFFER | e) Unit
set = runFn3 setImpl

foreign import unsafeAtImpl :: forall a e. Fn2 (ArrayView a) Int (Eff (arrayBuffer :: ARRAYBUFFER | e) Number)

-- | Fetch element at index.
unsafeAt :: forall a e. ArrayView a -> Int -> Eff (arrayBuffer :: ARRAYBUFFER | e) Number
unsafeAt = runFn2 unsafeAtImpl

foreign import hasIndexImpl :: forall a e. Fn2 (ArrayView a) Int (Eff (arrayBuffer :: ARRAYBUFFER | e) Boolean)

-- | Determine if a certain index is valid.
hasIndex :: forall a e. ArrayView a -> Int -> Eff (arrayBuffer :: ARRAYBUFFER | e) Boolean
hasIndex = runFn2 hasIndexImpl

-- | Fetch element at index.
at :: forall a e. ArrayView a -> Int -> Eff (arrayBuffer :: ARRAYBUFFER | e) (Maybe Number)
at a n = do
  indexExists <- a `hasIndex` n
  if indexExists
    then do
      element <- unsafeAt a n
      pure $ Just element
    else
      pure Nothing

-- | Turn typed array into an array.
foreign import toArray :: forall a e. ArrayView a -> Eff (arrayBuffer :: ARRAYBUFFER | e) (Array Number)

-- | Turn typed array into integer array.
foreign import toIntArray :: forall a e. ArrayView a -> Eff (arrayBuffer :: ARRAYBUFFER | e) (Array Int)
