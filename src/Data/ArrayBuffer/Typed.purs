-- | This module represents the functional bindings to JavaScript's `TypedArray` and other
-- | objects. See [MDN's spec](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray) for details.

module Data.ArrayBuffer.Typed
  ( polyFill
  , buffer
  , Offset, Length
  , byteOffset
  , byteLength
  , AProxy (..)
  , class BytesPerValue
  , bytesPerValue
  , length
  , class TypedArray
  , whole, remainder, part, empty, fromArray, all, any, fill, fillRemainder, fillPart, set, set'
  , map', traverse', traverse_', filter, foldlM', foldl', foldl1', foldrM', foldr', foldr1'
  , copyWithin, copyWithinPart
  , reverse
  , setTyped, setTyped'
  , copy, sliceRemainder, slice
  , sort
  , subArray, subArrayRemainder
  , toString
  , toString'
  , unsafeAt
  , hasIndex
  , at
  , toArray
  ) where

import Prelude
import Effect (Effect)
import Effect.Uncurried
  ( EffectFn4, EffectFn3, EffectFn2, EffectFn1
  , runEffectFn4, runEffectFn3, runEffectFn2, runEffectFn1
  , mkEffectFn1, mkEffectFn2)
import Effect.Unsafe (unsafePerformEffect)
import Data.Nullable (Nullable, toNullable)
import Data.ArrayBuffer.Types
  ( ArrayView, kind ArrayViewType, ArrayBuffer, ByteOffset, ByteLength
  , Float64Array, Float32Array
  , Uint8ClampedArray, Uint32Array, Uint16Array, Uint8Array, Int32Array, Int16Array, Int8Array
  , Float64, Float32
  , Uint8Clamped, Uint32, Uint16, Uint8, Int32, Int16, Int8)
import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3, mkFn2)
import Data.Maybe (Maybe(..))


-- | Lightweight polyfill for ie - see https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray#Methods_Polyfill
foreign import polyFill :: Effect Unit

-- | `ArrayBuffer` being mapped by the typed array.
foreign import buffer :: forall a. ArrayView a -> ArrayBuffer

-- | Represents the offset of this view from the start of its `ArrayBuffer`.
foreign import byteOffset :: forall a. ArrayView a -> ByteOffset

-- | Represents the length of this typed array, in bytes.
foreign import byteLength :: forall a. ArrayView a -> ByteLength

foreign import lengthImpl :: forall a. ArrayView a -> Length

data AProxy (a :: ArrayViewType) = AProxy

class BytesPerValue (a :: ArrayViewType) where
  bytesPerValue :: AProxy a -> Int

instance bytesPerValueUint8Clamped :: BytesPerValue Uint8Clamped where
  bytesPerValue AProxy = 1
instance bytesPerValueUint32 :: BytesPerValue Uint32 where
  bytesPerValue AProxy = 4
instance bytesPerValueUint16 :: BytesPerValue Uint16 where
  bytesPerValue AProxy = 2
instance bytesPerValueUint8 :: BytesPerValue Uint8 where
  bytesPerValue AProxy = 1
instance bytesPerValueInt32 :: BytesPerValue Int32 where
  bytesPerValue AProxy = 4
instance bytesPerValueInt16 :: BytesPerValue Int16 where
  bytesPerValue AProxy = 2
instance bytesPerValueInt8 :: BytesPerValue Int8 where
  bytesPerValue AProxy = 1

length :: forall a. BytesPerValue a => ArrayView a -> Int
length = lengthImpl


-- object creator implementations for each typed array

foreign import newUint8ClampedArray :: forall a. EffectFn1 a Uint8ClampedArray
foreign import newUint8ClampedArray2 :: EffectFn2 ArrayBuffer ByteOffset Uint8ClampedArray
foreign import newUint8ClampedArray3 :: EffectFn3 ArrayBuffer ByteOffset ByteLength Uint8ClampedArray


-- ----

foreign import everyImpl :: forall a b. Fn2 (ArrayView a) (b -> Boolean) Boolean
foreign import someImpl :: forall a b. Fn2 (ArrayView a) (b -> Boolean) Boolean

foreign import fillImpl :: forall a b. EffectFn2 (ArrayView a) b Unit
foreign import fillImpl2 :: forall a b. EffectFn3 (ArrayView a) b Offset Unit
foreign import fillImpl3 :: forall a b. EffectFn4 (ArrayView a) b Offset Offset Unit

foreign import mapImpl :: forall a b. EffectFn2 (ArrayView a) (EffectFn1 b b) (ArrayView a)
foreign import forEachImpl :: forall a b. EffectFn2 (ArrayView a) (EffectFn1 b Unit) Unit
foreign import filterImpl :: forall a b. Fn2 (ArrayView a) (b -> Boolean) (ArrayView a)
foreign import reduceImpl :: forall a b c. EffectFn3 (ArrayView a) (EffectFn2 c b c) c c
foreign import reduce1Impl :: forall a b. Fn2 (ArrayView a) (Fn2 b b b) b
foreign import reduceRightImpl :: forall a b c. EffectFn3 (ArrayView a) (EffectFn2 c b c) c c
foreign import reduceRight1Impl :: forall a b. Fn2 (ArrayView a) (Fn2 b b b) b


-- | Value-oriented array offset
type Offset = Int
-- | Value-oriented array length
type Length = Int


-- TODO use purescript-quotient
-- | Measured user-level values stored in each typed array
class TypedArray (a :: ArrayViewType) (t :: Type) | a -> t where
  -- | View mapping the whole `ArrayBuffer`.
  whole :: ArrayBuffer -> ArrayView a
  -- | View mapping the rest of an `ArrayBuffer` after an index.
  remainder :: ArrayBuffer -> Offset -> Effect (ArrayView a)
  -- | View mapping a region of the `ArrayBuffer`.
  part :: ArrayBuffer -> Offset -> Length -> Effect (ArrayView a)
  -- | Creates an empty typed array, where each value is assigned 0
  empty :: Length -> ArrayView a
  -- | Creates a typed array from an input array of values, to be binary serialized
  fromArray :: Array t -> ArrayView a
  -- | Test a predicate to pass on all values
  all :: (t -> Boolean) -> ArrayView a -> Boolean
  -- | Test a predicate to pass on any value
  any :: (t -> Boolean) -> ArrayView a -> Boolean
  -- | Fill the array with a value
  fill :: ArrayView a -> t -> Effect Unit
  -- | Fill the remainder of the array with a value
  fillRemainder :: ArrayView a -> t -> Offset -> Effect Unit
  -- | Fill part of the array with a value
  fillPart :: ArrayView a -> t -> Offset -> Offset -> Effect Unit
  -- | Stores multiple values into the typed array
  set :: ArrayView a -> Array t -> Effect Unit
  -- | Stores multiple values into the typed array, with offset
  set' :: ArrayView a -> Offset -> Array t -> Effect Unit
  -- | Maps a new value over the typed array, creating a new buffer and typed array as well.
  map' :: (t -> t) -> ArrayView a -> ArrayView a
  -- | Traverses over each value, returning a new one
  traverse' :: (t -> Effect t) -> ArrayView a -> Effect (ArrayView a)
  -- | Traverses over each value
  traverse_' :: (t -> Effect Unit) -> ArrayView a -> Effect Unit
  -- | Returns a new typed array with all values that pass the predicate
  filter :: (t -> Boolean) -> ArrayView a -> ArrayView a
  -- | Fetch element at index.
  unsafeAt :: ArrayView a -> Offset -> Effect t
  -- | Folding from the left
  foldlM' :: forall b. ArrayView a -> (b -> t -> Effect b) -> b -> Effect b
  -- | Assumes the typed array is non-empty
  foldl1' :: ArrayView a -> (t -> t -> t) -> t
  -- | Folding from the right
  foldrM' :: forall b. ArrayView a -> (t -> b -> Effect b) -> b -> Effect b
  -- | Assumes the typed array is non-empty
  foldr1' :: ArrayView a -> (t -> t -> t) -> t

instance typedArrayUint8Clamped :: TypedArray Uint8Clamped Int where
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
  map' f a = unsafePerformEffect (runEffectFn2 mapImpl a (mkEffectFn1 (pure <<< f)))
  traverse' f a = runEffectFn2 mapImpl a (mkEffectFn1 f)
  traverse_' f a = runEffectFn2 forEachImpl a (mkEffectFn1 f)
  filter p a = runFn2 filterImpl a p
  unsafeAt = runEffectFn2 unsafeAtImpl
  foldlM' a f = runEffectFn3 reduceImpl a (mkEffectFn2 f)
  foldl1' a f = runFn2 reduce1Impl a (mkFn2 f)
  foldrM' a f = runEffectFn3 reduceRightImpl a (mkEffectFn2 (flip f))
  foldr1' a f = runFn2 reduceRight1Impl a (mkFn2 (flip f))
-- instance typedArrayUint32 :: TypedArray Uint32 Number
-- instance typedArrayUint16 :: TypedArray Uint16 Int
-- instance typedArrayUint8 :: TypedArray Uint8 Int
-- instance typedArrayInt32 :: TypedArray Int32 Number
-- instance typedArrayInt16 :: TypedArray Int16 Int
-- instance typedArrayInt8 :: TypedArray Int8 Int


foldl' :: forall a b t. TypedArray a t => ArrayView a -> (b -> t -> b) -> b -> b
foldl' a f i = unsafePerformEffect (foldlM' a (\acc x -> pure (f acc x)) i)

foldr' :: forall a b t. TypedArray a t => ArrayView a -> (t -> b -> b) -> b -> b
foldr' a f i = unsafePerformEffect (foldrM' a (\x acc -> pure (f x acc)) i)


foreign import copyWithinImpl :: forall a. EffectFn3 (ArrayView a) Offset Offset Unit
foreign import copyWithinImpl3 :: forall a. EffectFn4 (ArrayView a) Offset Offset Offset Unit

-- | Internally copy values - see [MDN's spec](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/copyWithin) for details.
copyWithin :: forall a. ArrayView a -> Offset -> Offset -> Effect Unit
copyWithin = runEffectFn3 copyWithinImpl
copyWithinPart :: forall a. ArrayView a -> Offset -> Offset -> Offset -> Effect Unit
copyWithinPart = runEffectFn4 copyWithinImpl3

foreign import reverseImpl :: forall a. EffectFn1 (ArrayView a) Unit

-- | Reverses a typed array in-place.
reverse :: forall a. ArrayView a -> Effect Unit
reverse = runEffectFn1 reverseImpl

foreign import setImpl :: forall a b. EffectFn3 (ArrayView a) (Nullable Offset) b Unit


-- | Stores multiple values in the typed array, reading input values from the second typed array.
setTyped :: forall a. ArrayView a -> ArrayView a -> Effect Unit
setTyped a x = runEffectFn3 setImpl a (toNullable Nothing) x

-- | Stores multiple values in the typed array, reading input values from the second typed array, with offset.
setTyped' :: forall a. ArrayView a -> Offset -> ArrayView a -> Effect Unit
setTyped' a o x = runEffectFn3 setImpl a (toNullable (Just o)) x


-- | Copy the entire contents of the typed array into a new buffer.
foreign import copy :: forall a. ArrayView a -> ArrayView a
foreign import sliceRemainderImpl :: forall a. Fn2 (ArrayView a) Offset (ArrayView a)
foreign import sliceImpl :: forall a. Fn3 (ArrayView a) Offset Offset (ArrayView a)

-- | Copy the remainder of contents of the typed array into a new buffer, after some start index.
sliceRemainder :: forall a. ArrayView a -> Offset -> ArrayView a
sliceRemainder = runFn2 sliceRemainderImpl
-- | Copy part of the contents of a typed array into a new buffer, between some start and end indices.
slice :: forall a. ArrayView a -> Offset -> Offset -> ArrayView a
slice = runFn3 sliceImpl


foreign import sortImpl :: forall a. EffectFn1 (ArrayView a) Unit

-- | Sorts the values in-place
sort :: forall a. ArrayView a -> Effect Unit
sort = runEffectFn1 sortImpl


foreign import subArrayImpl :: forall a. Fn2 (ArrayView a) Offset (ArrayView a)
foreign import subArrayImpl2 :: forall a. Fn3 (ArrayView a) Offset Offset (ArrayView a)


-- | Returns a new typed array view of the same buffer, beginning at the index and ending at the second.
subArray :: forall a. ArrayView a -> Offset -> Offset -> ArrayView a
subArray = runFn3 subArrayImpl2
-- | Returns a new typed array view of the same buffer, beginning at the index
subArrayRemainder :: forall a. ArrayView a -> Offset -> ArrayView a
subArrayRemainder = runFn2 subArrayImpl


-- | Prints array to a comma-separated string - see [MDN's spec](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/toString) for details.
foreign import toString :: forall a. ArrayView a -> String

foreign import joinImpl :: forall a. Fn2 (ArrayView a) String String

-- | Prints array to a delimiter-separated string - see [MDN's spec](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/join) for details.
toString' :: forall a. ArrayView a -> String -> String
toString' = runFn2 joinImpl


foreign import unsafeAtImpl :: forall a b. EffectFn2 (ArrayView a) Offset b

foreign import hasIndexImpl :: forall a. Fn2 (ArrayView a) Offset Boolean

-- | Determine if a certain index is valid.
hasIndex :: forall a. ArrayView a -> Offset -> Boolean
hasIndex = runFn2 hasIndexImpl

-- | Fetch element at index.
at :: forall a t. TypedArray a t => ArrayView a -> Offset -> Maybe t
at a n = do
  if a `hasIndex` n
    then Just (unsafePerformEffect (unsafeAt a n))
    else Nothing

foreign import toArrayImpl :: forall a b. ArrayView a -> Array b

-- | Turn typed array into an array.
toArray :: forall a t. TypedArray a t => ArrayView a -> Array t
toArray = toArrayImpl
