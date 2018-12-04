## Module Data.ArrayBuffer.Typed

This module represents the functional bindings to JavaScript's `TypedArray` and other
objects. See [MDN's spec](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray) for details.

#### `polyFill`

``` purescript
polyFill :: Effect Unit
```

Lightweight polyfill for ie - see https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray#Methods_Polyfill

#### `buffer`

``` purescript
buffer :: forall a. ArrayView a -> ArrayBuffer
```

`ArrayBuffer` being mapped by the typed array.

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

#### `AProxy`

``` purescript
data AProxy (a :: ArrayViewType)
  = AProxy
```

#### `BytesPerValue`

``` purescript
class BytesPerValue (a :: ArrayViewType)  where
  bytesPerValue :: AProxy a -> Int
```

##### Instances
``` purescript
BytesPerValue Uint8Clamped
BytesPerValue Uint32
BytesPerValue Uint16
BytesPerValue Uint8
BytesPerValue Int32
BytesPerValue Int16
BytesPerValue Int8
```

#### `length`

``` purescript
length :: forall a. BytesPerValue a => ArrayView a -> Int
```

#### `TypedArray`

``` purescript
class TypedArray (a :: ArrayViewType) (t :: Type) | a -> t where
  whole :: ArrayBuffer -> ArrayView a
  remainder :: ArrayBuffer -> Offset -> Effect (ArrayView a)
  part :: ArrayBuffer -> Offset -> Length -> Effect (ArrayView a)
  empty :: Length -> ArrayView a
  fromArray :: Array t -> ArrayView a
  all :: (t -> Boolean) -> ArrayView a -> Boolean
  any :: (t -> Boolean) -> ArrayView a -> Boolean
  fill :: ArrayView a -> t -> Effect Unit
  fillRemainder :: ArrayView a -> t -> Offset -> Effect Unit
  fillPart :: ArrayView a -> t -> Offset -> Offset -> Effect Unit
  set :: ArrayView a -> Array t -> Effect Unit
  set' :: ArrayView a -> Offset -> Array t -> Effect Unit
  map' :: (t -> t) -> ArrayView a -> ArrayView a
  traverse' :: (t -> Effect t) -> ArrayView a -> Effect (ArrayView a)
  traverse_' :: (t -> Effect Unit) -> ArrayView a -> Effect Unit
  filter :: (t -> Boolean) -> ArrayView a -> ArrayView a
  unsafeAt :: ArrayView a -> Offset -> Effect t
```

Measured user-level values stored in each typed array

##### Instances
``` purescript
TypedArray Uint8Clamped Int
```

#### `copyWithin`

``` purescript
copyWithin :: forall a. ArrayView a -> Offset -> Offset -> Effect Unit
```

Internally copy values - see [MDN's spec](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/copyWithin) for details.

#### `copyWithinPart`

``` purescript
copyWithinPart :: forall a. ArrayView a -> Offset -> Offset -> Offset -> Effect Unit
```

#### `reverse`

``` purescript
reverse :: forall a. ArrayView a -> Effect Unit
```

Reverses a typed array in-place.

#### `setTyped`

``` purescript
setTyped :: forall a. ArrayView a -> ArrayView a -> Effect Unit
```

Stores multiple values in the typed array, reading input values from the second typed array.

#### `setTyped'`

``` purescript
setTyped' :: forall a. ArrayView a -> Offset -> ArrayView a -> Effect Unit
```

Stores multiple values in the typed array, reading input values from the second typed array, with offset.

#### `copy`

``` purescript
copy :: forall a. ArrayView a -> ArrayView a
```

Copy the entire contents of the typed array into a new buffer.

#### `sliceRemainder`

``` purescript
sliceRemainder :: forall a. ArrayView a -> Offset -> ArrayView a
```

Copy the remainder of contents of the typed array into a new buffer, after some start index.

#### `slice`

``` purescript
slice :: forall a. ArrayView a -> Offset -> Offset -> ArrayView a
```

Copy part of the contents of a typed array into a new buffer, between some start and end indices.

#### `sort`

``` purescript
sort :: forall a. ArrayView a -> Effect Unit
```

Sorts the values in-place

#### `subArray`

``` purescript
subArray :: forall a. ArrayView a -> Offset -> Offset -> ArrayView a
```

Returns a new typed array view of the same buffer, beginning at the index and ending at the second.

#### `subArrayRemainder`

``` purescript
subArrayRemainder :: forall a. ArrayView a -> Offset -> ArrayView a
```

Returns a new typed array view of the same buffer, beginning at the index

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

#### `toArray`

``` purescript
toArray :: forall a t. TypedArray a t => ArrayView a -> Array t
```

Turn typed array into an array.


