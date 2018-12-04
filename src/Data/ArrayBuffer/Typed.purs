-- | This module represents the functional bindings to JavaScript's `TypedArray` and other
-- | objects. See [MDN's spec](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray) for details.

module Data.ArrayBuffer.Typed
  ( polyFill
  , buffer
  , byteOffset
  , byteLength
  , AProxy (..)
  , class BytesPer
  , bytesPer
  , length
  , class ValuesPer
  , whole, remainder, part, empty, fromArray, all, any, fill, fillRemainder, fillPart, set, set'
  , map'
  , copyWithin, copyWithinPart
  , reverse
  , setTyped, setTyped'
  , copy, sliceRemainder, slice
  , sort
  , subArray, subArrayRemainder
  , toString
  , unsafeAt
  , hasIndex
  , at
  , toArray
  , toIntArray
  ) where

import Prelude
import Effect (Effect)
import Effect.Uncurried
  ( EffectFn4, EffectFn3, EffectFn2, EffectFn1
  , runEffectFn4, runEffectFn3, runEffectFn2, runEffectFn1)
import Effect.Unsafe (unsafePerformEffect)
import Data.Nullable (Nullable, toNullable)
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

class BytesPer (a :: ArrayViewType) where
  bytesPer :: AProxy a -> Int

instance bytesPerUint8Clamped :: BytesPer Uint8Clamped where
  bytesPer AProxy = 1
instance bytesPerUint32 :: BytesPer Uint32 where
  bytesPer AProxy = 4
instance bytesPerUint16 :: BytesPer Uint16 where
  bytesPer AProxy = 2
instance bytesPerUint8 :: BytesPer Uint8 where
  bytesPer AProxy = 1
instance bytesPerInt32 :: BytesPer Int32 where
  bytesPer AProxy = 4
instance bytesPerInt16 :: BytesPer Int16 where
  bytesPer AProxy = 2
instance bytesPerInt8 :: BytesPer Int8 where
  bytesPer AProxy = 1

length :: forall a. BytesPer a => ArrayView a -> Int
length = lengthImpl


foreign import newUint8ClampedArray :: forall a. EffectFn1 a Uint8ClampedArray
foreign import newUint8ClampedArray2 :: EffectFn2 ArrayBuffer ByteOffset Uint8ClampedArray
foreign import newUint8ClampedArray3 :: EffectFn3 ArrayBuffer ByteOffset ByteLength Uint8ClampedArray


foreign import everyImpl :: forall a b. Fn2 (ArrayView a) (b -> Boolean) Boolean
foreign import someImpl :: forall a b. Fn2 (ArrayView a) (b -> Boolean) Boolean

foreign import fillImpl :: forall a b. EffectFn2 (ArrayView a) b Unit
foreign import fillImpl2 :: forall a b. EffectFn3 (ArrayView a) b ByteOffset Unit
foreign import fillImpl3 :: forall a b. EffectFn4 (ArrayView a) b ByteOffset ByteOffset Unit

foreign import mapImpl :: forall a b. Fn2 (ArrayView a) (b -> b) (ArrayView a)


-- TODO use purescript-quotient
-- | Measured user-level values stored in each typed array
class ValuesPer (a :: ArrayViewType) (t :: Type) | a -> t where
  -- | View mapping the whole `ArrayBuffer`.
  whole :: ArrayBuffer -> ArrayView a
  -- | View mapping the rest of an `ArrayBuffer` after an index.
  remainder :: ArrayBuffer -> ByteOffset -> Effect (ArrayView a)
  -- | View mapping a region of the `ArrayBuffer`.
  part :: ArrayBuffer -> ByteOffset -> ByteLength -> Effect (ArrayView a)
  -- | Creates an empty typed array, where each value is assigned 0
  empty :: Int -> ArrayView a
  -- | Creates a typed array from an input array of values, to be binary serialized
  fromArray :: Array t -> ArrayView a
  -- | Test a predicate to pass on all values
  all :: (t -> Boolean) -> ArrayView a -> Boolean
  -- | Test a predicate to pass on any value
  any :: (t -> Boolean) -> ArrayView a -> Boolean
  -- | Fill the array with a value
  fill :: ArrayView a -> t -> Effect Unit
  -- | Fill the remainder of the array with a value
  fillRemainder :: ArrayView a -> t -> ByteOffset -> Effect Unit
  -- | Fill part of the array with a value
  fillPart :: ArrayView a -> t -> ByteOffset -> ByteOffset -> Effect Unit
  -- | Stores multiple values into the typed array
  set :: ArrayView a -> Array t -> Effect Unit
  -- | Stores multiple values into the typed array, with offset
  set' :: ArrayView a -> ByteOffset -> Array t -> Effect Unit
  -- | Maps a new value over the typed array, creating a new buffer and typed array as well.
  map' :: (t -> t) -> ArrayView a -> ArrayView a

instance valuesPerUint8Clamped :: ValuesPer Uint8Clamped Int where
  whole a = unsafePerformEffect (runEffectFn1 newUint8ClampedArray a)
  remainder = runEffectFn2 newUint8ClampedArray2
  part = runEffectFn3 newUint8ClampedArray3
  empty n = unsafePerformEffect (runEffectFn1 newUint8ClampedArray n)
  fromArray a = unsafePerformEffect (runEffectFn1 newUint8ClampedArray a)
  all p a = runFn2 everyImpl a p
  any p a = runFn2 someImpl a p
  fill = runEffectFn2 fillImpl
  fillRemainder = runEffectFn3 fillImpl2
  fillPart = runEffectFn4 fillImpl3
  set a x = runEffectFn3 setImpl a (toNullable Nothing) x
  set' a o x = runEffectFn3 setImpl a (toNullable (Just o)) x
  map' f a = runFn2 mapImpl a f
-- instance valuesPerUint32 :: ValuesPer Uint32 Number
-- instance valuesPerUint16 :: ValuesPer Uint16 Int
-- instance valuesPerUint8 :: ValuesPer Uint8 Int
-- instance valuesPerInt32 :: ValuesPer Int32 Number
-- instance valuesPerInt16 :: ValuesPer Int16 Int
-- instance valuesPerInt8 :: ValuesPer Int8 Int


foreign import copyWithinImpl :: forall a. EffectFn3 (ArrayView a) ByteOffset ByteOffset Unit
foreign import copyWithinImpl3 :: forall a. EffectFn4 (ArrayView a) ByteOffset ByteOffset ByteOffset Unit

-- | Internally copy values - see [MDN's spec](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/copyWithin) for details.
copyWithin :: forall a. ArrayView a -> ByteOffset -> ByteOffset -> Effect Unit
copyWithin = runEffectFn3 copyWithinImpl
copyWithinPart :: forall a. ArrayView a -> ByteOffset -> ByteOffset -> ByteOffset -> Effect Unit
copyWithinPart = runEffectFn4 copyWithinImpl3

foreign import reverseImpl :: forall a. EffectFn1 (ArrayView a) Unit

-- | Reverses a typed array in-place.
reverse :: forall a. ArrayView a -> Effect Unit
reverse = runEffectFn1 reverseImpl

foreign import setImpl :: forall a b. EffectFn3 (ArrayView a) (Nullable ByteOffset) b Unit


-- | Stores multiple values in the typed array, reading input values from the second typed array.
setTyped :: forall a. ArrayView a -> ArrayView a -> Effect Unit
setTyped a x = runEffectFn3 setImpl a (toNullable Nothing) x

-- | Stores multiple values in the typed array, reading input values from the second typed array, with offset.
setTyped' :: forall a. ArrayView a -> ByteOffset -> ArrayView a -> Effect Unit
setTyped' a o x = runEffectFn3 setImpl a (toNullable (Just o)) x


-- | Copy the entire contents of the typed array into a new buffer.
foreign import copy :: forall a. ArrayView a -> ArrayView a
foreign import sliceRemainderImpl :: forall a. Fn2 (ArrayView a) ByteOffset (ArrayView a)
foreign import sliceImpl :: forall a. Fn3 (ArrayView a) ByteOffset ByteOffset (ArrayView a)

-- | Copy the remainder of contents of the typed array into a new buffer, after some start index.
sliceRemainder :: forall a. ArrayView a -> ByteOffset -> ArrayView a
sliceRemainder = runFn2 sliceRemainderImpl
-- | Copy part of the contents of a typed array into a new buffer, between some start and end indices.
slice :: forall a. ArrayView a -> ByteOffset -> ByteOffset -> ArrayView a
slice = runFn3 sliceImpl


foreign import sortImpl :: forall a. EffectFn1 (ArrayView a) Unit

-- | Sorts the values in-place
sort :: forall a. ArrayView a -> Effect Unit
sort = runEffectFn1 sortImpl


foreign import subArrayImpl :: forall a. Fn2 (ArrayView a) ByteOffset (ArrayView a)
foreign import subArrayImpl2 :: forall a. Fn3 (ArrayView a) ByteOffset ByteOffset (ArrayView a)


-- | Returns a new typed array view of the same buffer, beginning at the index and ending at the second.
subArray :: forall a. ArrayView a -> ByteOffset -> ByteOffset -> ArrayView a
subArray = runFn3 subArrayImpl2
-- | Returns a new typed array view of the same buffer, beginning at the index
subArrayRemainder :: forall a. ArrayView a -> ByteOffset -> ArrayView a
subArrayRemainder = runFn2 subArrayImpl


-- | Prints array to a comma-separated string - see [MDN's spec](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/toString) for details.
foreign import toString :: forall a. ArrayView a -> String

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
