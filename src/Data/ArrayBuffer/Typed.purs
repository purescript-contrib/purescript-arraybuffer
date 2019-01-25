-- | This module represents the functional bindings to JavaScript's `TypedArray` and other
-- | objects. See [MDN's spec](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray) for details.

module Data.ArrayBuffer.Typed
  ( polyFill
  , Offset, Length, Range
  , buffer, byteOffset, byteLength, length
  , class TypedArray
  , create, whole, remainder, part, empty, fromArray
  , fill, set, setTyped, copyWithin
  , map, traverse, traverse_, filter
  , mapWithIndex, traverseWithIndex, traverseWithIndex_, filterWithIndex
  , sort, reverse
  , elem
  , all, any
  , allWithIndex, anyWithIndex
  , unsafeAt, hasIndex, at, (!)
  , foldlM, foldl1M, foldl, foldl1, foldrM, foldr1M, foldr, foldr1
  , find, findIndex, indexOf, lastIndexOf
  , slice, subArray
  , toString, toString', toArray
  ) where

import Prelude

import Data.Array (length) as A
import Data.ArrayBuffer.Types (ArrayView, kind ArrayViewType, ArrayBuffer, ByteOffset, ByteLength, Float64Array, Float32Array, Uint8ClampedArray, Uint32Array, Uint16Array, Uint8Array, Int32Array, Int16Array, Int8Array, Float64, Float32, Uint8Clamped, Uint32, Uint16, Uint8, Int32, Int16, Int8)
import Data.ArrayBuffer.ValueMapping (class BinaryValue, class BytesPerValue)
import Data.Function.Uncurried (Fn2, Fn3, mkFn2, runFn2, runFn3)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (Nullable, notNull, null, toMaybe, toNullable)
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (class Nat, toInt')
import Data.UInt (UInt)
import Data.Float32 (Float32) as F
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn4, mkEffectFn2, mkEffectFn3, runEffectFn1, runEffectFn2, runEffectFn3, runEffectFn4)
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy(..))


-- | Lightweight polyfill for ie - see https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray#Methods_Polyfill
foreign import polyFill :: Effect Unit

-- | `ArrayBuffer` being mapped by the typed array.
foreign import buffer :: forall a. ArrayView a -> ArrayBuffer

-- | Represents the offset of this view from the start of its `ArrayBuffer`.
foreign import byteOffset :: forall a. ArrayView a -> ByteOffset

-- | Represents the length of this typed array, in bytes.
foreign import byteLength :: forall a. ArrayView a -> ByteLength

foreign import lengthImpl :: forall a. ArrayView a -> Length

length :: forall a. ArrayView a -> Length
length = lengthImpl


-- object creator implementations for each typed array

foreign import newUint8ClampedArray :: forall a. Fn3 a (Nullable ByteOffset) (Nullable ByteLength) Uint8ClampedArray
foreign import newUint32Array :: forall a. Fn3 a (Nullable ByteOffset) (Nullable ByteLength) Uint32Array
foreign import newUint16Array :: forall a. Fn3 a (Nullable ByteOffset) (Nullable ByteLength) Uint16Array
foreign import newUint8Array :: forall a. Fn3 a (Nullable ByteOffset) (Nullable ByteLength) Uint8Array
foreign import newInt32Array :: forall a. Fn3 a (Nullable ByteOffset) (Nullable ByteLength) Int32Array
foreign import newInt16Array :: forall a. Fn3 a (Nullable ByteOffset) (Nullable ByteLength) Int16Array
foreign import newInt8Array :: forall a. Fn3 a (Nullable ByteOffset) (Nullable ByteLength) Int8Array
foreign import newFloat32Array :: forall a. Fn3 a (Nullable ByteOffset) (Nullable ByteLength) Float32Array
foreign import newFloat64Array :: forall a. Fn3 a (Nullable ByteOffset) (Nullable ByteLength) Float64Array


-- ----

foreign import everyImpl :: forall a b. Fn2 (ArrayView a) (Fn2 b Offset Boolean) Boolean
foreign import someImpl :: forall a b. Fn2 (ArrayView a) (Fn2 b Offset Boolean) Boolean

foreign import fillImpl :: forall a b. EffectFn4 (ArrayView a) b (Nullable Offset) (Nullable Offset) Unit

foreign import mapImpl :: forall a b. EffectFn2 (ArrayView a) (EffectFn2 b Offset b) (ArrayView a)
foreign import forEachImpl :: forall a b. EffectFn2 (ArrayView a) (EffectFn2 b Offset Unit) Unit
foreign import filterImpl :: forall a b. Fn2 (ArrayView a) (Fn2 b Offset Boolean) (ArrayView a)
foreign import includesImpl :: forall a b. Fn3 (ArrayView a) b (Nullable Offset) Boolean
foreign import reduceImpl :: forall a b c. EffectFn3 (ArrayView a) (EffectFn3 c b Offset c) c c
foreign import reduce1Impl :: forall a b. EffectFn2 (ArrayView a) (EffectFn3 b b Offset b) b
foreign import reduceRightImpl :: forall a b c. EffectFn3 (ArrayView a) (EffectFn3 c b Offset c) c c
foreign import reduceRight1Impl :: forall a b. EffectFn2 (ArrayView a) (EffectFn3 b b Offset b) b
foreign import findImpl :: forall a b. Fn2 (ArrayView a) (Fn2 b Offset Boolean) (Nullable b)
foreign import findIndexImpl :: forall a b. Fn2 (ArrayView a) (Fn2 b Offset Boolean) (Nullable Offset)
foreign import indexOfImpl :: forall a b. Fn3 (ArrayView a) b (Nullable Offset) (Nullable Offset)
foreign import lastIndexOfImpl :: forall a b. Fn3 (ArrayView a) b (Nullable Offset) (Nullable Offset)


-- | Value-oriented array offset
type Offset = Int
-- | Value-oriented array length
type Length = Int

-- | Represents a range of indices, where if omitted, it represents the whole span.
-- | If only the second argument is omitted, then it represents the remainder of the span after the first index.
type Range = Maybe (Tuple Offset (Maybe Offset))


-- TODO use purescript-quotient
-- | Typeclass that associates a measured user-level type with a typed array.
-- |
-- | #### Creation
-- |
-- | - `whole`, `remainder`, and `part` are methods for building a typed array accessible interface
-- |   on top of an existing `ArrayBuffer` - Note, `part` and `remainder` may behave unintuitively -
-- |   when the operation is isomorphic to `whole`, the new TypedArray uses the same buffer as the input,
-- |   but not when the portion is a sub-array of the original buffer, a new one is made with
-- |   `Data.ArrayBuffer.ArrayBuffer.slice`.
-- | - `empty` and `fromArray` are methods for creating pure typed arrays
-- |
-- | #### Modification
-- |
-- | - `fill`, `set`, and `setTyped` are methods for assigning values from external sources
-- | - `map` and `traverse` allow you to create a new array from the existing values in another
-- | - `copyWithin` allows you to set values to the array that exist in other parts of the array
-- | - `filter` creates a new array without the values that don't pass a predicate
-- | - `reverse` modifies an existing array in-place, with all values reversed
-- | - `sort` modifies an existing array in-place, with all values sorted
-- |
-- | #### Access
-- |
-- | - `elem`, `all`, and `any` are functions for testing the contents of an array
-- | - `unsafeAt`, `hasIndex`, and `at` are used to get values from an array, with an offset
-- | - `foldr`, `foldrM`, `foldr1`, `foldr1M`, `foldl`, `foldlM`, `foldl1`, `foldl1M` all can reduce an array
-- | - `find` and `findIndex` are searching functions via a predicate
-- | - `indexOf` and `lastIndexOf` are searching functions via equality
-- | - `slice` returns a new typed array on the same array buffer content as the input
-- | - `subArray` returns a new typed array with a separate array buffer
-- | - `toString` prints to a CSV, `toString'` allows you to supply the delimiter
-- | - `toArray` returns an array of numeric values
class BinaryValue a t <= TypedArray (a :: ArrayViewType) (t :: Type) | a -> t where
  create :: forall x. Fn3 x (Nullable ByteOffset) (Nullable ByteLength) (ArrayView a)

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
whole :: forall a t. TypedArray a t => ArrayBuffer -> ArrayView a
whole a = runFn3 create a null null

-- | View mapping the rest of an `ArrayBuffer` after an index.
remainder :: forall a b t. TypedArray a t => Nat b => BytesPerValue a b => ArrayBuffer -> Offset -> ArrayView a
remainder a x = remainder' a o
  where o = x * toInt' (Proxy :: Proxy b)

remainder' :: forall a t. TypedArray a t => ArrayBuffer -> ByteOffset -> ArrayView a
remainder' a x = runFn3 create a (notNull x) null

-- | View mapping a region of the `ArrayBuffer`.
part :: forall a b t. TypedArray a t => Nat b => BytesPerValue a b => ArrayBuffer -> Offset -> Length -> ArrayView a
part a x y = part' a o y
  where o = x * toInt' (Proxy :: Proxy b)

part' :: forall a t. TypedArray a t => ArrayBuffer -> ByteOffset -> Length -> ArrayView a
part' a x y = runFn3 create a (notNull x) (notNull y)

-- | Creates an empty typed array, where each value is assigned 0
empty :: forall a t. TypedArray a t => Length -> ArrayView a
empty n = runFn3 create n null null

-- | Creates a typed array from an input array of values, to be binary serialized
fromArray :: forall a t. TypedArray a t => Array t -> ArrayView a
fromArray a = runFn3 create a null null

-- | Fill the array with a value
fill :: forall a t. TypedArray a t => ArrayView a -> t -> Range -> Effect Unit
fill a x mz = case mz of
  Nothing -> runEffectFn4 fillImpl a x null null
  Just (Tuple s me) -> runEffectFn4 fillImpl a x (notNull s) (toNullable me)

-- | Stores multiple values into the typed array
set :: forall a t. TypedArray a t => ArrayView a -> Maybe Offset -> Array t -> Effect Boolean
set = setInternal A.length

ap1 :: forall a b c. (a -> c) -> (a -> b -> c)
ap1 f = \x _ -> f x


-- | Maps a new value over the typed array, creating a new buffer and
-- | typed array as well.
map :: forall a t. TypedArray a t => (t -> t) -> ArrayView a -> ArrayView a
map = mapWithIndex' <<< ap1

-- | Apply a function to each element in an array, supplying a
-- | generated zero-based index integer along with the element,
-- | creating a typed array with the new elements
mapWithIndex :: forall a t. TypedArray a t => (Offset -> t -> t) -> ArrayView a -> ArrayView a
mapWithIndex = mapWithIndex' <<< flip

mapWithIndex' :: forall a t. TypedArray a t => (t -> Offset -> t) -> ArrayView a -> ArrayView a
mapWithIndex' f a = unsafePerformEffect (runEffectFn2 mapImpl a (mkEffectFn2 (\x o -> pure (f x o))))

-- | Traverses over each value, returning a new one
traverse :: forall a t. TypedArray a t => (t -> Effect t) -> ArrayView a -> Effect (ArrayView a)
traverse = traverseWithIndex' <<< ap1

-- | Traverses over each value, returning a new one
traverseWithIndex :: forall a t. TypedArray a t => (Offset -> t -> Effect t) -> ArrayView a -> Effect (ArrayView a)
traverseWithIndex = traverseWithIndex' <<< flip

traverseWithIndex' :: forall a t. TypedArray a t => (t -> Offset -> Effect t) -> ArrayView a -> Effect (ArrayView a)
traverseWithIndex' f a = runEffectFn2 mapImpl a (mkEffectFn2 f)

-- | Traverses over each value
traverse_ :: forall a t. TypedArray a t => (t -> Effect Unit) -> ArrayView a -> Effect Unit
traverse_ = traverseWithIndex_' <<< ap1

-- | Traverses over each value
traverseWithIndex_ :: forall a t. TypedArray a t => (Offset -> t -> Effect Unit) -> ArrayView a -> Effect Unit
traverseWithIndex_ = traverseWithIndex_' <<< flip

traverseWithIndex_' :: forall a t. TypedArray a t => (t -> Offset -> Effect Unit) -> ArrayView a -> Effect Unit
traverseWithIndex_' f a = runEffectFn2 forEachImpl a (mkEffectFn2 f)

-- | Test a predicate to pass on all values
all :: forall a t. TypedArray a t => (t -> Boolean) -> ArrayView a -> Boolean
all = every <<< ap1

allWithIndex :: forall a t. TypedArray a t => (Offset -> t -> Boolean) -> ArrayView a -> Boolean
allWithIndex = every <<< flip

every :: forall a t. TypedArray a t => (t -> Offset -> Boolean) -> ArrayView a -> Boolean
every p a = runFn2 everyImpl a (mkFn2 p)

-- | Test a predicate to pass on any value
any :: forall a t. TypedArray a t => (t -> Boolean) -> ArrayView a -> Boolean
any = some <<< ap1

anyWithIndex :: forall a t. TypedArray a t => (Offset -> t -> Boolean) -> ArrayView a -> Boolean
anyWithIndex = some <<< flip

some :: forall a t. TypedArray a t => (t -> Offset -> Boolean) -> ArrayView a -> Boolean
some p a = runFn2 someImpl a (mkFn2 p)

-- | Returns a new typed array with all values that pass the predicate
filter :: forall a t. TypedArray a t => (t -> Boolean) -> ArrayView a -> ArrayView a
filter = filterWithIndex' <<< ap1

filterWithIndex :: forall a t. TypedArray a t => (Offset -> t -> Boolean) -> ArrayView a -> ArrayView a
filterWithIndex = filterWithIndex' <<< flip

filterWithIndex' :: forall a t. TypedArray a t => (t -> Offset -> Boolean) -> ArrayView a -> ArrayView a
filterWithIndex' p a = runFn2 filterImpl a (mkFn2 p)

-- | Tests if a value is an element of the typed array
elem :: forall a t. TypedArray a t => t -> Maybe Offset -> ArrayView a -> Boolean
elem x mo a = runFn3 includesImpl a x (toNullable mo)

-- | Fetch element at index.
unsafeAt :: forall a t. TypedArray a t => Partial => ArrayView a -> Offset -> t
unsafeAt a o = runFn2 unsafeAtImpl a o

-- | Folding from the left
foldlM :: forall a t b. TypedArray a t => (b -> t -> Offset -> Effect b) -> b -> ArrayView a -> Effect b
foldlM f i a = runEffectFn3 reduceImpl a (mkEffectFn3 f) i

-- | Assumes the typed array is non-empty
foldl1M :: forall a t. TypedArray a t => (t -> t -> Offset -> Effect t) -> ArrayView a -> Effect t
foldl1M f a = runEffectFn2 reduce1Impl a (mkEffectFn3 f)

-- | Folding from the right
foldrM :: forall a t b. TypedArray a t => (t -> b -> Offset -> Effect b) -> b -> ArrayView a -> Effect b
foldrM f i a = runEffectFn3 reduceRightImpl a (mkEffectFn3 (\acc x o -> f x acc o)) i

-- | Assumes the typed array is non-empty
foldr1M :: forall a t. TypedArray a t => (t -> t -> Offset -> Effect t) -> ArrayView a -> Effect t
foldr1M f a = runEffectFn2 reduceRight1Impl a (mkEffectFn3 (\acc x o -> f x acc o))

-- | Returns the first value satisfying the predicate
find :: forall a t. TypedArray a t => (t -> Boolean) -> ArrayView a -> Maybe t
find = findWithIndex' <<< ap1

findWithIndex :: forall a t. TypedArray a t => (Offset -> t -> Boolean) -> ArrayView a -> Maybe t
findWithIndex = findWithIndex' <<< flip

findWithIndex' :: forall a t. TypedArray a t => (t -> Offset -> Boolean) -> ArrayView a -> Maybe t
findWithIndex' f a = toMaybe (runFn2 findImpl a (mkFn2 f))

-- | Returns the first index of the value satisfying the predicate
findIndex :: forall a t. TypedArray a t => (t -> Offset -> Boolean) -> ArrayView a -> Maybe Offset
findIndex f a = toMaybe (runFn2 findIndexImpl a (mkFn2 f))

-- | Returns the first index of the element, if it exists, from the left
indexOf :: forall a t. TypedArray a t => t -> Maybe Offset -> ArrayView a -> Maybe Offset
indexOf x mo a = toMaybe (runFn3 indexOfImpl a x (toNullable mo))

-- | Returns the first index of the element, if it exists, from the right
lastIndexOf :: forall a t. TypedArray a t => t -> Maybe Offset -> ArrayView a -> Maybe Offset
lastIndexOf x mo a = toMaybe (runFn3 lastIndexOfImpl a x (toNullable mo))

foldl :: forall a b t. TypedArray a t => (b -> t -> b) -> b -> ArrayView a -> b
foldl f = foldlWithIndex' (\a x _ -> f a x)

foldlWithIndex :: forall a b t. TypedArray a t => (Offset -> b -> t -> b) -> b -> ArrayView a -> b
foldlWithIndex f = foldlWithIndex' (\a x o -> f o a x)

foldlWithIndex' :: forall a b t. TypedArray a t => (b -> t -> Offset -> b) -> b -> ArrayView a -> b
foldlWithIndex' f i = unsafePerformEffect <<< foldlM (\a x o -> pure (f a x o)) i

foldr :: forall a b t. TypedArray a t => (t -> b -> b) -> b -> ArrayView a -> b
foldr f = foldrWithIndex' (\a x _ -> f a x)

foldrWithIndex :: forall a b t. TypedArray a t => (Offset -> t -> b -> b) -> b -> ArrayView a -> b
foldrWithIndex f = foldrWithIndex' (\a x o -> f o a x)

foldrWithIndex' :: forall a b t. TypedArray a t => (t -> b -> Offset -> b) -> b -> ArrayView a -> b
foldrWithIndex' f i = unsafePerformEffect <<< foldrM (\x a o -> pure (f x a o)) i

foldl1 :: forall a t. TypedArray a t => (t -> t -> t) -> ArrayView a -> t
foldl1 f = foldl1WithIndex (\_ a x -> f a x)

foldl1WithIndex :: forall a t. TypedArray a t => (Offset -> t -> t -> t) -> ArrayView a -> t
foldl1WithIndex f = unsafePerformEffect <<< foldl1M (\acc x o -> pure (f o acc x))

foldr1 :: forall a t. TypedArray a t => (t -> t -> t) -> ArrayView a -> t
foldr1 f = foldr1WithIndex (\_ a x -> f a x)

foldr1WithIndex :: forall a t. TypedArray a t => (Offset -> t -> t -> t) -> ArrayView a -> t
foldr1WithIndex f = unsafePerformEffect <<< foldr1M (\x a o -> pure (f o x a))

foreign import copyWithinImpl :: forall a. EffectFn4 (ArrayView a) Offset Offset (Nullable Offset) Unit

-- | Internally copy values - see [MDN's spec](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/copyWithin) for details.
copyWithin :: forall a. ArrayView a -> Offset -> Offset -> Maybe Offset -> Effect Unit
copyWithin a t s me = runEffectFn4 copyWithinImpl a t s (toNullable me)

foreign import reverseImpl :: forall a. EffectFn1 (ArrayView a) Unit

-- | Reverses a typed array in-place.
reverse :: forall a. ArrayView a -> Effect Unit
reverse a = runEffectFn1 reverseImpl a

foreign import setImpl :: forall a b. EffectFn3 (ArrayView a) Offset b Unit

setInternal :: forall a b. (b -> Length) -> ArrayView a -> Maybe Offset -> b -> Effect Boolean
setInternal lenfn a mo b =
  let o = fromMaybe 0 mo
  in if o >= 0 && lenfn b <= length a - o
     then runEffectFn3 setImpl a o b *> pure true
     else pure false


-- | Stores multiple values in the typed array, reading input values from the second typed array.
setTyped :: forall a. ArrayView a -> Maybe Offset -> ArrayView a -> Effect Boolean
setTyped = setInternal length


-- | Copy the entire contents of the typed array into a new buffer.
foreign import sliceImpl :: forall a. Fn3 (ArrayView a) (Nullable Offset) (Nullable Offset) (ArrayView a)

-- | Copy part of the contents of a typed array into a new buffer, between some start and end indices.
slice :: forall a. ArrayView a -> Range -> ArrayView a
slice a mz = case mz of
  Nothing -> runFn3 sliceImpl a null null
  Just (Tuple s me) -> runFn3 sliceImpl a (notNull s) (toNullable me)

foreign import sortImpl :: forall a. EffectFn1 (ArrayView a) Unit

-- | Sorts the values in-place
sort :: forall a. ArrayView a -> Effect Unit
sort a = runEffectFn1 sortImpl a


foreign import subArrayImpl :: forall a. Fn3 (ArrayView a) (Nullable Offset) (Nullable Offset) (ArrayView a)

-- | Returns a new typed array view of the same buffer, beginning at the index and ending at the second.
-- |
-- | **Note**: there is really peculiar behavior with `subArray` - if the first offset argument is omitted, or
-- | is `0`, and likewise if the second argument is the length of the array, then the "sub-array" is actually a
-- | mutable replica of the original array - the sub-array reference reflects mutations to the original array.
-- | However, when the sub-array is is actually a smaller contiguous portion of the array, then it behaves
-- | purely, because JavaScript interally calls `Data.ArrayBuffer.ArrayBuffer.slice`.
subArray :: forall a. ArrayView a -> Range -> ArrayView a
subArray a mz = case mz of
  Nothing -> runFn3 subArrayImpl a null null
  Just (Tuple s me) -> runFn3 subArrayImpl a (notNull s) (toNullable me)

-- | Prints array to a comma-separated string - see [MDN's spec](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/toString) for details.
foreign import toString :: forall a. ArrayView a -> String

foreign import joinImpl :: forall a. Fn2 (ArrayView a) String String

-- | Prints array to a delimiter-separated string - see [MDN's spec](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/join) for details.
toString' :: forall a. ArrayView a -> String -> String
toString' a s = runFn2 joinImpl a s


foreign import unsafeAtImpl :: forall a b. Fn2 (ArrayView a) Offset b

foreign import hasIndexImpl :: forall a. Fn2 (ArrayView a) Offset Boolean

-- | Determine if a certain index is valid.
hasIndex :: forall a. ArrayView a -> Offset -> Boolean
hasIndex a o = runFn2 hasIndexImpl a o

-- | Fetch element at index.
at :: forall a t. TypedArray a t => ArrayView a -> Offset -> Maybe t
at a n = if a `hasIndex` n
         then Just (unsafePartial (unsafeAt a n))
         else Nothing

infixl 3 at as !


foreign import toArrayImpl :: forall a b. ArrayView a -> Array b

-- | Turn typed array into an array.
toArray :: forall a t. TypedArray a t => ArrayView a -> Array t
toArray = toArrayImpl
