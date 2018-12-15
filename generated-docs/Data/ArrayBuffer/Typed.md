## Module Data.ArrayBuffer.Typed

This module represents the functional bindings to JavaScript's `TypedArray` and other
objects. See [MDN's spec](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray) for details.

#### `polyFill`

``` purescript
polyFill :: Effect Unit
```

Lightweight polyfill for ie - see https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray#Methods_Polyfill

#### `Offset`

``` purescript
type Offset = Int
```

Value-oriented array offset

#### `Length`

``` purescript
type Length = Int
```

Value-oriented array length

#### `Range`

``` purescript
type Range = Maybe (Tuple Offset (Maybe Offset))
```

Represents a range of indicies, where if omitted, it represents the whole span.
If only the second argument is omitted, then it represents the remainder of the span after the first index.

#### `buffer`

``` purescript
buffer :: forall a. ArrayView a -> ArrayBuffer
```

`ArrayBuffer` being mapped by the typed array.

#### `byteOffset`

``` purescript
byteOffset :: forall a. ArrayView a -> ByteOffset
```

Represents the offset of this view from the start of its `ArrayBuffer`.

#### `byteLength`

``` purescript
byteLength :: forall a. ArrayView a -> ByteLength
```

Represents the length of this typed array, in bytes.

#### `length`

``` purescript
length :: forall a b. BytesPerValue a b => ArrayView a -> Int
```

#### `TypedArray`

``` purescript
class (BinaryValue a t) <= TypedArray (a :: ArrayViewType) (t :: Type) | a -> t where
  whole :: ArrayBuffer -> ArrayView a
  remainder :: ArrayBuffer -> ByteOffset -> Effect (ArrayView a)
  part :: ArrayBuffer -> ByteOffset -> Length -> Effect (ArrayView a)
  empty :: Length -> ArrayView a
  fromArray :: Array t -> ArrayView a
  fill :: ArrayView a -> t -> Range -> Effect Unit
  set :: ArrayView a -> Maybe Offset -> Array t -> Effect Unit
  map :: (t -> Offset -> t) -> ArrayView a -> ArrayView a
  traverse :: (t -> Offset -> Effect t) -> ArrayView a -> Effect (ArrayView a)
  traverse_ :: (t -> Offset -> Effect Unit) -> ArrayView a -> Effect Unit
  all :: (t -> Offset -> Effect Boolean) -> ArrayView a -> Effect Boolean
  any :: (t -> Offset -> Effect Boolean) -> ArrayView a -> Effect Boolean
  filter :: (t -> Offset -> Effect Boolean) -> ArrayView a -> Effect (ArrayView a)
  elem :: t -> Maybe Offset -> ArrayView a -> Boolean
  unsafeAt :: Offset -> ArrayView a -> Effect t
  foldlM :: forall b. (b -> t -> Offset -> Effect b) -> b -> ArrayView a -> Effect b
  foldl1M :: (t -> t -> Offset -> Effect t) -> ArrayView a -> Effect t
  foldrM :: forall b. (t -> b -> Offset -> Effect b) -> b -> ArrayView a -> Effect b
  foldr1M :: (t -> t -> Offset -> Effect t) -> ArrayView a -> Effect t
  find :: (t -> Offset -> Effect Boolean) -> ArrayView a -> Effect (Maybe t)
  findIndex :: (t -> Offset -> Effect Boolean) -> ArrayView a -> Effect (Maybe Offset)
  indexOf :: t -> Maybe Offset -> ArrayView a -> Maybe Offset
  lastIndexOf :: t -> Maybe Offset -> ArrayView a -> Maybe Offset
```

Typeclass that associates a measured user-level type with a typed array.

#### Creation

- `whole`, `remainder`, and `part` are methods for building a typed array accessible interface
  on top of an existing `ArrayBuffer` - Note, `part` and `remainder` may behave unintuitively -
  when the operation is isomorphic to `whole`, the new TypedArray uses the same buffer as the input,
  but not when the portion is a sub-array of the original buffer, a new one is made with
  `Data.ArrayBuffer.ArrayBuffer.slice`.
- `empty` and `fromArray` are methods for creating pure typed arrays

#### Modification

- `fill`, `set`, and `setTyped` are methods for assigning values from external sources
- `map` and `traverse` allow you to create a new array from the existing values in another
- `copyWithin` allows you to set values to the array that exist in other parts of the array
- `filter` creates a new array without the values that don't pass a predicate
- `reverse` modifies an existing array in-place, with all values reversed
- `sort` modifies an existing array in-place, with all values sorted

#### Access

- `elem`, `all`, and `any` are functions for testing the contents of an array
- `unsafeAt`, `hasIndex`, and `at` are used to get values from an array, with an offset
- `foldr`, `foldrM`, `foldr1`, `foldr1M`, `foldl`, `foldlM`, `foldl1`, `foldl1M` all can reduce an array
- `find` and `findIndex` are searching functions via a predicate
- `indexOf` and `lastIndexOf` are searching functions via equality
- `slice` returns a new typed array on the same array buffer content as the input
- `subArray` returns a new typed array with a separate array buffer
- `toString` prints to a CSV, `toString'` allows you to supply the delimiter
- `toArray` returns an array of numeric values

##### Instances
``` purescript
TypedArray Uint8Clamped UInt
TypedArray Uint32 UInt
TypedArray Uint16 UInt
TypedArray Uint8 UInt
TypedArray Int32 Int
TypedArray Int16 Int
TypedArray Int8 Int
TypedArray Float32 Number
TypedArray Float64 Number
```

#### `setTyped`

``` purescript
setTyped :: forall a. ArrayView a -> Maybe Offset -> ArrayView a -> Effect Unit
```

Stores multiple values in the typed array, reading input values from the second typed array.

#### `copyWithin`

``` purescript
copyWithin :: forall a. ArrayView a -> Offset -> Offset -> Maybe Offset -> Effect Unit
```

Internally copy values - see [MDN's spec](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/copyWithin) for details.

#### `sort`

``` purescript
sort :: forall a. ArrayView a -> Effect Unit
```

Sorts the values in-place

#### `reverse`

``` purescript
reverse :: forall a. ArrayView a -> Effect Unit
```

Reverses a typed array in-place.

#### `hasIndex`

``` purescript
hasIndex :: forall a. ArrayView a -> Offset -> Boolean
```

Determine if a certain index is valid.

#### `at`

``` purescript
at :: forall a t. TypedArray a t => ArrayView a -> Offset -> Maybe t
```

Fetch element at index.

#### `(!)`

``` purescript
infixl 3 at as !
```

#### `foldl`

``` purescript
foldl :: forall a b t. TypedArray a t => (b -> t -> Offset -> b) -> b -> ArrayView a -> b
```

#### `foldl1`

``` purescript
foldl1 :: forall a t. TypedArray a t => (t -> t -> Offset -> t) -> ArrayView a -> t
```

#### `foldr`

``` purescript
foldr :: forall a b t. TypedArray a t => (t -> b -> Offset -> b) -> b -> ArrayView a -> b
```

#### `foldr1`

``` purescript
foldr1 :: forall a t. TypedArray a t => (t -> t -> Offset -> t) -> ArrayView a -> t
```

#### `slice`

``` purescript
slice :: forall a. ArrayView a -> Range -> ArrayView a
```

Copy part of the contents of a typed array into a new buffer, between some start and end indices.

#### `subArray`

``` purescript
subArray :: forall a. ArrayView a -> Range -> ArrayView a
```

Returns a new typed array view of the same buffer, beginning at the index and ending at the second.

**Note**: there is really peculiar behavior with `subArray` - if the first offset argument is omitted, or
is `0`, and likewise if the second argument is the length of the array, then the "sub-array" is actually a
mutable replica of the original array - the sub-array reference reflects mutations to the original array.
However, when the sub-array is is actually a smaller contiguous portion of the array, then it behaves
purely, because JavaScript interally calls `Data.ArrayBuffer.ArrayBuffer.slice`.

#### `toString`

``` purescript
toString :: forall a. ArrayView a -> String
```

Prints array to a comma-separated string - see [MDN's spec](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/toString) for details.

#### `toString'`

``` purescript
toString' :: forall a. ArrayView a -> String -> String
```

Prints array to a delimiter-separated string - see [MDN's spec](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/join) for details.

#### `toArray`

``` purescript
toArray :: forall a t. TypedArray a t => ArrayView a -> Array t
```

Turn typed array into an array.


