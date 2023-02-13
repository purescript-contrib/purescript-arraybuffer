-- | This module represents the functional bindings to JavaScript's `TypedArray` and other
-- | objects. See [MDN's spec](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray) for details.
-- |
-- | #### Creation
-- |
-- | - `whole`, `remainder`, and `part` are functions for building a typed array accessible interface
-- |   on top of an existing `ArrayBuffer`
-- | - `empty` and `fromArray` are functions for creating pure typed arrays
-- |
-- | #### Modification
-- |
-- | - `fill`, `set`, and `setTyped` are functions for assigning values from external sources
-- | - `map` and `traverse` allow you to create a new array from the existing values in another
-- | - `copyWithin` allows you to set values to the array that exist in other parts of the array
-- | - `filter` creates a new array without the values that don't pass a predicate
-- | - `reverse` modifies an existing array in-place, with all values reversed
-- | - `sort` modifies an existing array in-place, with all values sorted
-- |
-- | #### Access
-- |
-- | - `elem`, `all`, and `any` are functions for testing the contents of an array
-- | - `unsafeAt`, `hasIndex`, and `at` are used to get values from an array, with an index
-- | - `foldr`, `foldrM`, `foldr1`, `foldr1M`, `foldl`, `foldlM`, `foldl1`, `foldl1M` all can reduce an array
-- | - `find` and `findIndex` are searching functions via a predicate
-- | - `indexOf` and `lastIndexOf` are searching functions via equality
-- | - `slice` returns a new typed array with a new copied underlying `ArrayBuffer`
-- | - `subArray` returns a new typed array view of the same `ArrayBuffer`
-- | - `toString` prints to a CSV, `join` allows you to supply the delimiter
-- | - `toArray` returns an array of numeric values

module Data.ArrayBuffer.Typed
  ( Index
  , Length
  , buffer
  , byteOffset
  , byteLength
  , length
  , compare
  , eq
  , class TypedArray
  , create
  , whole
  , remainder
  , part
  , empty
  , fromArray
  , fill
  , set
  , setTyped
  , copyWithin
  , map
  , traverse
  , traverse_
  , filter
  , mapWithIndex
  , traverseWithIndex
  , traverseWithIndex_
  , filterWithIndex
  , sort
  , reverse
  , elem
  , all
  , any
  , allWithIndex
  , anyWithIndex
  , unsafeAt
  , hasIndex
  , at
  , (!)
  , reduce
  , reduce1
  , foldl
  , foldl1
  , reduceRight
  , reduceRight1
  , foldr
  , foldr1
  , foldlWithIndex
  , foldrWithIndex
  , find
  , findIndex
  , findWithIndex
  , indexOf
  , lastIndexOf
  , slice
  , subArray
  , toString
  , join
  , toArray
  ) where

import Data.Array (length) as A
import Data.ArrayBuffer.Types (ArrayView, ArrayViewType, ArrayBuffer, ByteOffset, ByteLength, Float64Array, Float32Array, Uint8ClampedArray, Uint32Array, Uint16Array, Uint8Array, Int32Array, Int16Array, Int8Array, Float64, Float32, Uint8Clamped, Uint32, Uint16, Uint8, Int32, Int16, Int8)
import Data.ArrayBuffer.ValueMapping (class BinaryValue, class BytesPerType, byteWidth)
import Data.Float32 (Float32) as F
import Data.Function.Uncurried (Fn2, Fn3, mkFn2, runFn2, runFn3)
import Data.Maybe (Maybe, fromMaybe)
import Data.Nullable (Nullable, notNull, null, toMaybe, toNullable)
import Data.UInt (UInt)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn4, mkEffectFn2, mkEffectFn3, runEffectFn1, runEffectFn2, runEffectFn3, runEffectFn4)
import Effect.Unsafe (unsafePerformEffect)
import Prelude (class Eq, class Ord, Ordering, Unit, flip, pure, ($), (&&), (*), (*>), (-), (<$>), (<*>), (<<<), (<=), (>=))
import Prelude as Prelude
import Type.Proxy (Proxy(..))

-- | `ArrayBuffer` being mapped by the typed array.
foreign import buffer :: forall a. ArrayView a -> ArrayBuffer

-- | Represents the offset of this view from the start of its `ArrayBuffer`.
foreign import byteOffset :: forall a. ArrayView a -> ByteOffset

-- | Represents the length of this typed array, in bytes.
foreign import byteLength :: forall a. ArrayView a -> ByteLength

-- | Represents the number of elements in this typed array.
length :: forall a. ArrayView a -> Length
length = lengthImpl

foreign import lengthImpl :: forall a. ArrayView a -> Length

-- object creator implementations for each typed array

foreign import newUint8ClampedArray :: forall a. EffectFn3 a (Nullable ByteOffset) (Nullable ByteLength) Uint8ClampedArray
foreign import newUint32Array :: forall a. EffectFn3 a (Nullable ByteOffset) (Nullable ByteLength) Uint32Array
foreign import newUint16Array :: forall a. EffectFn3 a (Nullable ByteOffset) (Nullable ByteLength) Uint16Array
foreign import newUint8Array :: forall a. EffectFn3 a (Nullable ByteOffset) (Nullable ByteLength) Uint8Array
foreign import newInt32Array :: forall a. EffectFn3 a (Nullable ByteOffset) (Nullable ByteLength) Int32Array
foreign import newInt16Array :: forall a. EffectFn3 a (Nullable ByteOffset) (Nullable ByteLength) Int16Array
foreign import newInt8Array :: forall a. EffectFn3 a (Nullable ByteOffset) (Nullable ByteLength) Int8Array
foreign import newFloat32Array :: forall a. EffectFn3 a (Nullable ByteOffset) (Nullable ByteLength) Float32Array
foreign import newFloat64Array :: forall a. EffectFn3 a (Nullable ByteOffset) (Nullable ByteLength) Float64Array

-- | Value-oriented array index.
type Index = Int
-- | Value-oriented array length.
type Length = Int

class BinaryValue a t <= TypedArray (a :: ArrayViewType) (t :: Type) | a -> t where
  create :: forall x. EffectFn3 x (Nullable ByteOffset) (Nullable ByteLength) (ArrayView a)

instance typedArrayUint8Clamped :: TypedArray Uint8Clamped UInt where
  create = newUint8ClampedArray

instance typedArrayUint32 :: TypedArray Uint32 UInt where
  create = newUint32Array

instance typedArrayUint16 :: TypedArray Uint16 UInt where
  create = newUint16Array

instance typedArrayUint8 :: TypedArray Uint8 UInt where
  create = newUint8Array

instance typedArrayInt32 :: TypedArray Int32 Int where
  create = newInt32Array

instance typedArrayInt16 :: TypedArray Int16 Int where
  create = newInt16Array

instance typedArrayInt8 :: TypedArray Int8 Int where
  create = newInt8Array

instance typedArrayFloat32 :: TypedArray Float32 F.Float32 where
  create = newFloat32Array

instance typedArrayFloat64 :: TypedArray Float64 Number where
  create = newFloat64Array

-- | View mapping the whole `ArrayBuffer`.
whole :: forall a t. TypedArray a t => ArrayBuffer -> Effect (ArrayView a)
whole a = runEffectFn3 create a null null

-- | View mapping the rest of an `ArrayBuffer` after an index.
remainder :: forall a b t. TypedArray a t => BytesPerType b => ArrayBuffer -> Index -> Effect (ArrayView a)
remainder a x = remainder' a o
  where
  o = x * byteWidth (Proxy :: Proxy b)

remainder' :: forall a t. TypedArray a t => ArrayBuffer -> ByteOffset -> Effect (ArrayView a)
remainder' a x = runEffectFn3 create a (notNull x) null

-- | View mapping a region of the `ArrayBuffer`.
part :: forall a t. TypedArray a t => BytesPerType a => ArrayBuffer -> Index -> Length -> Effect (ArrayView a)
part a x y = part' a o y
  where
  o = x * byteWidth (Proxy :: Proxy a)

-- | The ByteOffset must be aligned.
-- | https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray#byteoffset_must_be_aligned
part' :: forall a t. TypedArray a t => ArrayBuffer -> ByteOffset -> Length -> Effect (ArrayView a)
part' a x y = runEffectFn3 create a (notNull x) (notNull y)

-- | Creates an empty typed array, where each value is assigned 0.
empty :: forall a t. TypedArray a t => Length -> Effect (ArrayView a)
empty n = runEffectFn3 create n null null

-- | Creates a typed array from an input array of values, to be binary serialized.
fromArray :: forall a t. TypedArray a t => Array t -> Effect (ArrayView a)
fromArray a = runEffectFn3 create a null null

-- | Fill the array with a value.
fill :: forall a t. TypedArray a t => t -> Index -> Index -> ArrayView a -> Effect Unit
fill x s e a = runEffectFn4 fillImpl x s e a

foreign import fillImpl :: forall a b. EffectFn4 b Index Index (ArrayView a) Unit

-- | Stores multiple values into the typed array.
set :: forall a t. TypedArray a t => ArrayView a -> Maybe Index -> Array t -> Effect Boolean
set = setInternal A.length

ap1 :: forall a b c. (a -> c) -> (a -> b -> c)
ap1 f = \x _ -> f x

-- | Maps a new value over the typed array, creating a new `ArrayBuffer` and
-- | typed array as well.
map :: forall a t. TypedArray a t => (t -> t) -> ArrayView a -> ArrayView a
map = mapWithIndex' <<< ap1

-- | Apply a function to each element in an array, supplying a
-- | generated zero-based index integer along with the element,
-- | creating a typed array with the new elements.
mapWithIndex :: forall a t. TypedArray a t => (Index -> t -> t) -> ArrayView a -> ArrayView a
mapWithIndex = mapWithIndex' <<< flip

mapWithIndex' :: forall a t. TypedArray a t => (t -> Index -> t) -> ArrayView a -> ArrayView a
mapWithIndex' f a = unsafePerformEffect (runEffectFn2 mapImpl a (mkEffectFn2 \x o -> pure (f x o)))

foreign import mapImpl :: forall a b. EffectFn2 (ArrayView a) (EffectFn2 b Index b) (ArrayView a)

-- | Traverses over each value, returning a new one.
traverse :: forall a t. TypedArray a t => (t -> Effect t) -> ArrayView a -> Effect (ArrayView a)
traverse = traverseWithIndex' <<< ap1

-- | Traverses over each value, returning a new one.
traverseWithIndex :: forall a t. TypedArray a t => (Index -> t -> Effect t) -> ArrayView a -> Effect (ArrayView a)
traverseWithIndex = traverseWithIndex' <<< flip

traverseWithIndex' :: forall a t. TypedArray a t => (t -> Index -> Effect t) -> ArrayView a -> Effect (ArrayView a)
traverseWithIndex' f a = runEffectFn2 mapImpl a (mkEffectFn2 f)

-- | Traverses over each value.
traverse_ :: forall a t. TypedArray a t => (t -> Effect Unit) -> ArrayView a -> Effect Unit
traverse_ = traverseWithIndex_' <<< ap1

-- | Traverses over each value.
traverseWithIndex_ :: forall a t. TypedArray a t => (Index -> t -> Effect Unit) -> ArrayView a -> Effect Unit
traverseWithIndex_ = traverseWithIndex_' <<< flip

traverseWithIndex_' :: forall a t. TypedArray a t => (t -> Index -> Effect Unit) -> ArrayView a -> Effect Unit
traverseWithIndex_' f a = runEffectFn2 forEachImpl a (mkEffectFn2 f)

foreign import forEachImpl :: forall a b. EffectFn2 (ArrayView a) (EffectFn2 b Index Unit) Unit

-- | Test a predicate to pass on all values.
all :: forall a t. TypedArray a t => (t -> Boolean) -> ArrayView a -> Effect Boolean
all = every <<< ap1

-- | Test a predicate to pass on all values. The predicate function
-- | receives the index and the element.
allWithIndex :: forall a t. TypedArray a t => (Index -> t -> Boolean) -> ArrayView a -> Effect Boolean
allWithIndex = every <<< flip

every :: forall a t. TypedArray a t => (t -> Index -> Boolean) -> ArrayView a -> Effect Boolean
every p a = runEffectFn2 everyImpl a (mkFn2 p)

foreign import everyImpl :: forall a b. EffectFn2 (ArrayView a) (Fn2 b Index Boolean) Boolean

-- | Test a predicate to pass on any value.
any :: forall a t. TypedArray a t => (t -> Boolean) -> ArrayView a -> Effect Boolean
any = some <<< ap1

-- | Test a predicate (that receives also an index) to pass on any value.
anyWithIndex :: forall a t. TypedArray a t => (Index -> t -> Boolean) -> ArrayView a -> Effect Boolean
anyWithIndex = some <<< flip

some :: forall a t. TypedArray a t => (t -> Index -> Boolean) -> ArrayView a -> Effect Boolean
some p a = runEffectFn2 someImpl a (mkFn2 p)

foreign import someImpl :: forall a b. EffectFn2 (ArrayView a) (Fn2 b Index Boolean) Boolean

-- | Returns a new typed array with all values that pass the predicate.
filter :: forall a t. TypedArray a t => (t -> Boolean) -> ArrayView a -> Effect (ArrayView a)
filter = filterWithIndex' <<< ap1

-- | Returns a new typed array with all values that pass the
-- | predicate. The predicate function receives the index and the
-- | element.
filterWithIndex :: forall a t. TypedArray a t => (Index -> t -> Boolean) -> ArrayView a -> Effect (ArrayView a)
filterWithIndex = filterWithIndex' <<< flip

filterWithIndex' :: forall a t. TypedArray a t => (t -> Index -> Boolean) -> ArrayView a -> Effect (ArrayView a)
filterWithIndex' p a = runEffectFn2 filterImpl a (mkFn2 p)

foreign import filterImpl :: forall a b. EffectFn2 (ArrayView a) (Fn2 b Index Boolean) (ArrayView a)

-- | Tests if a value is an element of the typed array.
elem :: forall a t. TypedArray a t => t -> Maybe Index -> ArrayView a -> Effect Boolean
elem x mo a = runEffectFn3 includesImpl a x (toNullable mo)

foreign import includesImpl :: forall a b. EffectFn3 (ArrayView a) b (Nullable Index) Boolean

-- | Fetch element at index.
unsafeAt :: forall a t. TypedArray a t => Partial => ArrayView a -> Index -> Effect t
unsafeAt a o = runEffectFn2 unsafeAtImpl a o

-- | Folding from the left.
reduce :: forall a t b. TypedArray a t => (b -> t -> Index -> Effect b) -> b -> ArrayView a -> Effect b
reduce f i a = runEffectFn3 reduceImpl a (mkEffectFn3 f) i

foreign import reduceImpl :: forall a b c. EffectFn3 (ArrayView a) (EffectFn3 c b Index c) c c

-- | Folding from the left. Assumes the typed array is non-empty.
reduce1 :: forall a t. Partial => TypedArray a t => (t -> t -> Index -> Effect t) -> ArrayView a -> Effect t
reduce1 f a = runEffectFn2 reduce1Impl a (mkEffectFn3 f)

foreign import reduce1Impl :: forall a b. EffectFn2 (ArrayView a) (EffectFn3 b b Index b) b

-- | Folding from the right.
reduceRight :: forall a t b. TypedArray a t => (t -> b -> Index -> Effect b) -> b -> ArrayView a -> Effect b
reduceRight f i a = runEffectFn3 reduceRightImpl a (mkEffectFn3 \acc x o -> f x acc o) i

foreign import reduceRightImpl :: forall a b c. EffectFn3 (ArrayView a) (EffectFn3 c b Index c) c c

-- | Folding from the right. Assumes the typed array is non-empty.
reduceRight1 :: forall a t. Partial => TypedArray a t => (t -> t -> Index -> Effect t) -> ArrayView a -> Effect t
reduceRight1 f a = runEffectFn2 reduceRight1Impl a (mkEffectFn3 \acc x o -> f x acc o)

foreign import reduceRight1Impl :: forall a b. EffectFn2 (ArrayView a) (EffectFn3 b b Index b) b

-- | Returns the first value satisfying the predicate.
find :: forall a t. TypedArray a t => (t -> Boolean) -> ArrayView a -> Effect (Maybe t)
find = findWithIndex' <<< ap1

-- | Returns the first value satisfying the predicate. The predicate
-- | receives the index and the element at that index.
findWithIndex :: forall a t. TypedArray a t => (Index -> t -> Boolean) -> ArrayView a -> Effect (Maybe t)
findWithIndex = findWithIndex' <<< flip

findWithIndex' :: forall a t. TypedArray a t => (t -> Index -> Boolean) -> ArrayView a -> Effect (Maybe t)
findWithIndex' f a = toMaybe <$> runEffectFn2 findImpl a (mkFn2 f)

foreign import findImpl :: forall a b. EffectFn2 (ArrayView a) (Fn2 b Index Boolean) (Nullable b)

-- | Returns the first index of the value satisfying the predicate.
findIndex :: forall a t. TypedArray a t => (t -> Index -> Boolean) -> ArrayView a -> Effect (Maybe Index)
findIndex f a = toMaybe <$> runEffectFn2 findIndexImpl a (mkFn2 f)

foreign import findIndexImpl :: forall a b. EffectFn2 (ArrayView a) (Fn2 b Index Boolean) (Nullable Index)

-- | Returns the first index of the element, if it exists, from the left.
indexOf :: forall a t. TypedArray a t => t -> Maybe Index -> ArrayView a -> Effect (Maybe Index)
indexOf x mo a = toMaybe <$> runEffectFn3 indexOfImpl a x (toNullable mo)

foreign import indexOfImpl :: forall a b. EffectFn3 (ArrayView a) b (Nullable Index) (Nullable Index)

-- | Returns the first index of the element, if it exists, from the right.
lastIndexOf :: forall a t. TypedArray a t => t -> Maybe Index -> ArrayView a -> Effect (Maybe Index)
lastIndexOf x mo a = toMaybe <$> runEffectFn3 lastIndexOfImpl a x (toNullable mo)

foreign import lastIndexOfImpl :: forall a b. EffectFn3 (ArrayView a) b (Nullable Index) (Nullable Index)

-- | Fold a list from the left, accumulating the result using the
-- | specified function.
foldl :: forall a b t. TypedArray a t => (b -> t -> b) -> b -> ArrayView a -> Effect b
foldl f = reduce \a x _ -> pure $ f a x

-- | Fold a list from the left, accumulating the result using the
-- | supplied function. The accumulating function receives the index,
-- | the accumulated value and the element.
foldlWithIndex :: forall a b t. TypedArray a t => (Index -> b -> t -> b) -> b -> ArrayView a -> Effect b
foldlWithIndex f = reduce \a x o -> pure $ f o a x

-- | Fold a list from the right, accumulating the result using the
-- | specified function.
foldr :: forall a b t. TypedArray a t => (t -> b -> b) -> b -> ArrayView a -> Effect b
foldr f = reduceRight \a x _ -> pure $ f a x

-- | Fold a list from the right, accumulating the result using the
-- | supplied function. The accumulating function receives the index,
-- | the element and the accumulated value.
foldrWithIndex :: forall a b t. TypedArray a t => (Index -> t -> b -> b) -> b -> ArrayView a -> Effect b
foldrWithIndex f = reduceRight \a x o -> pure $ f o a x

-- | Folding from the left. Assumes the typed array is non-empty.
foldl1 :: forall a t. Partial => TypedArray a t => (t -> t -> t) -> ArrayView a -> Effect t
foldl1 f = reduce1 \a x _ -> pure $ f a x

-- | Folding from the right. Assumes the typed array is non-empty.
foldr1 :: forall a t. Partial => TypedArray a t => (t -> t -> t) -> ArrayView a -> Effect t
foldr1 f = reduceRight1 \x a _ -> pure $ f a x

-- | Internally copy values - see [MDN's spec](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/copyWithin) for details.
copyWithin :: forall a. ArrayView a -> Index -> Index -> Maybe Index -> Effect Unit
copyWithin a t s me = runEffectFn4 copyWithinImpl a t s (toNullable me)

foreign import copyWithinImpl :: forall a. EffectFn4 (ArrayView a) Index Index (Nullable Index) Unit

-- | Reverses a typed array in-place.
reverse :: forall a. ArrayView a -> Effect Unit
reverse a = runEffectFn1 reverseImpl a

foreign import reverseImpl :: forall a. EffectFn1 (ArrayView a) Unit

setInternal :: forall a b. (b -> Length) -> ArrayView a -> Maybe Index -> b -> Effect Boolean
setInternal lenfn a mo b = do
  let o = fromMaybe 0 mo
  if o >= 0 && lenfn b <= length a - o then
    runEffectFn3 setImpl a o b *> pure true
  else
    pure false

foreign import setImpl :: forall a b. EffectFn3 (ArrayView a) Index b Unit

-- | Stores multiple values in the typed array, reading input values from the second typed array.
setTyped :: forall a. ArrayView a -> Maybe Index -> ArrayView a -> Effect Boolean
setTyped = setInternal length

-- | Copy part of the contents of a typed array into a new `ArrayBuffer`,
-- | between the start and end indices.
slice :: forall a. Index -> Index -> ArrayView a -> Effect (ArrayView a)
slice s e a = runEffectFn3 sliceImpl a s e

foreign import sliceImpl :: forall a. EffectFn3 (ArrayView a) Index Index (ArrayView a)

-- | Sorts the values in-place.
sort :: forall a. ArrayView a -> Effect Unit
sort a = runEffectFn1 sortImpl a

foreign import sortImpl :: forall a. EffectFn1 (ArrayView a) Unit

-- | Returns a new typed array view of the same `ArrayBuffer`, beginning at
-- | the index and ending at the second.
subArray :: forall a. Index -> Index -> ArrayView a -> ArrayView a
subArray s e a = runFn3 subArrayImpl a s e

foreign import subArrayImpl :: forall a. Fn3 (ArrayView a) Index Index (ArrayView a)

-- | Prints array to a comma-separated string - see [MDN's spec](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/toString) for details.
toString :: forall a. ArrayView a -> Effect String
toString a = runEffectFn1 toStringImpl a

foreign import toStringImpl :: forall a. EffectFn1 (ArrayView a) String

-- | Prints array to a delimiter-separated string - see [MDN's spec](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/join) for details.
join :: forall a. String -> ArrayView a -> Effect String
join s a = runEffectFn2 joinImpl a s

foreign import joinImpl :: forall a. EffectFn2 (ArrayView a) String String

-- | Determine if a certain index is valid.
hasIndex :: forall a. ArrayView a -> Index -> Boolean
hasIndex a o = runFn2 hasIndexImpl a o

foreign import hasIndexImpl :: forall a. Fn2 (ArrayView a) Index Boolean

-- | Fetch element at index.
at :: forall a t. TypedArray a t => ArrayView a -> Index -> Effect (Maybe t)
at a n = toMaybe <$> runEffectFn2 unsafeAtImpl a n

foreign import unsafeAtImpl :: forall a b. EffectFn2 (ArrayView a) Index b

infixl 3 at as !

-- | Turn typed array into an array.
toArray :: forall a t. TypedArray a t => ArrayView a -> Effect (Array t)
toArray a = runEffectFn1 toArrayImpl a

foreign import toArrayImpl :: forall a b. EffectFn1 (ArrayView a) (Array b)

-- | Compare 2 typed arrays.
compare :: forall a t. TypedArray a t => Ord t => ArrayView a -> ArrayView a -> Effect Ordering
compare a b = Prelude.compare <$> toArray a <*> toArray b

-- | Equality test for typed arrays.
eq :: forall a t. TypedArray a t => Eq t => ArrayView a -> ArrayView a -> Effect Boolean
eq a b = Prelude.eq <$> toArray a <*> toArray b
