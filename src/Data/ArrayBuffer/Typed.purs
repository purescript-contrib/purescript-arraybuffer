-- | This module represents the functional bindings to JavaScript's `TypedArray` and other
-- | objects. See [MDN's spec](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray) for details.

module Data.ArrayBuffer.Typed
  ( polyFill
  , buffer
  , byteOffset
  , byteLength
  , AProxy (..)
  , class Bytes
  , bytesPer
  , length
  , set
  , unsafeAt
  , hasIndex
  , at
  , toArray
  , toIntArray
  ) where

import Prelude
import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Data.ArrayBuffer.Types
  ( ArrayView, kind ArrayViewType, ArrayBuffer, ByteOffset, ByteLength
  , Float64Array, Float32Array
  , Uint8ClampedArray, Uint32Array, Uint16Array, Uint8Array, Int32Array, Int16Array, Int8Array
  , Float64, Float32
  , Uint8Clamped, Uint32, Uint16, Uint8, Int32, Int16, Int8)
import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Data.Maybe (Maybe(..))


-- | Lightweight polyfill for ie - see https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray#Methods_Polyfill
foreign import polyFill :: Effect Unit

-- | `ArrayBuffer` being mapped by the typed array.
foreign import buffer :: forall a. ArrayView a -> ArrayBuffer

-- | Represents the offset of this view from the start of its `ArrayBuffer`.
foreign import byteOffset :: forall a. ArrayView a -> ByteOffset

-- | Represents the length of this typed array, in bytes.
foreign import byteLength :: forall a. ArrayView a -> ByteLength

foreign import lengthImpl :: forall a. ArrayView a -> Int

data AProxy (a :: ArrayViewType) = AProxy

class Bytes (a :: ArrayViewType) where
  bytesPer :: AProxy a -> Int

instance bytesUint8Clamped :: Bytes Uint8Clamped where
  bytesPer AProxy = 1
instance bytesUint32 :: Bytes Uint32 where
  bytesPer AProxy = 4
instance bytesUint16 :: Bytes Uint16 where
  bytesPer AProxy = 2
instance bytesUint8 :: Bytes Uint8 where
  bytesPer AProxy = 1
instance bytesInt32 :: Bytes Int32 where
  bytesPer AProxy = 4
instance bytesInt16 :: Bytes Int16 where
  bytesPer AProxy = 2
instance bytesInt8 :: Bytes Int8 where
  bytesPer AProxy = 1

length :: forall a. Bytes a => ArrayView a -> Int
length = lengthImpl


foreign import setImpl :: forall a. Fn3 (ArrayView a) ByteOffset (ArrayView a) (Effect Unit)

-- | Stores multiple values in the last typed array, reading input values from ther first typed array.
set :: forall a. ArrayView a -> ByteOffset -> ArrayView a -> Effect Unit
set = runFn3 setImpl

foreign import unsafeAtImpl :: forall a. EffectFn2 (ArrayView a) Int Number

-- | Fetch element at index.
unsafeAt :: forall a. ArrayView a -> Int -> Effect Number
unsafeAt = runEffectFn2 unsafeAtImpl

foreign import hasIndexImpl :: forall a. Fn2 (ArrayView a) Int Boolean

-- | Determine if a certain index is valid.
hasIndex :: forall a. ArrayView a -> Int -> Boolean
hasIndex = runFn2 hasIndexImpl

-- | Fetch element at index.
at :: forall a. ArrayView a -> Int -> Effect (Maybe Number)
at a n = do
  if a `hasIndex` n
    then do
      element <- unsafeAt a n
      pure $ Just element
    else
      pure Nothing

-- | Turn typed array into an array.
foreign import toArray :: forall a. ArrayView a -> Array Number

-- | Turn typed array into integer array.
foreign import toIntArray :: forall a. ArrayView a -> Array Int
