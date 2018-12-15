## Module Data.ArrayBuffer.DataView

This module represents the functional bindings to JavaScript's `DataView`
objects. See [MDN's spec](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DataView) for details.

#### `whole`

``` purescript
whole :: ArrayBuffer -> DataView
```

View mapping the whole `ArrayBuffer`.

#### `remainder`

``` purescript
remainder :: ArrayBuffer -> ByteOffset -> Effect DataView
```

View mapping the rest of an `ArrayBuffer` after an index.

#### `part`

``` purescript
part :: ArrayBuffer -> ByteOffset -> ByteLength -> Effect DataView
```

View mapping a region of the `ArrayBuffer`.

#### `buffer`

``` purescript
buffer :: DataView -> ArrayBuffer
```

`ArrayBuffer` being mapped by the view.

#### `byteOffset`

``` purescript
byteOffset :: DataView -> ByteOffset
```

Represents the offset of this view from the start of its `ArrayBuffer`.

#### `byteLength`

``` purescript
byteLength :: DataView -> ByteLength
```

Represents the length of this view.

#### `DVProxy`

``` purescript
data DVProxy (a :: ArrayViewType) (e :: Endianness)
  = DVProxy
```

#### `Endianness`

``` purescript
kind Endianness
```

#### `BE`

``` purescript
data BE :: Endianness
```

##### Instances
``` purescript
(BytesPerValue Int8 b, Nat b) => DataView Int8 BE Int
(BytesPerValue Int16 b, Nat b) => DataView Int16 BE Int
(BytesPerValue Int32 b, Nat b) => DataView Int32 BE Int
(BytesPerValue Uint8 b, Nat b) => DataView Uint8 BE UInt
(BytesPerValue Uint16 b, Nat b) => DataView Uint16 BE UInt
(BytesPerValue Uint32 b, Nat b) => DataView Uint32 BE UInt
(BytesPerValue Float32 b, Nat b) => DataView Float32 BE Number
(BytesPerValue Float64 b, Nat b) => DataView Float64 BE Number
```

#### `LE`

``` purescript
data LE :: Endianness
```

##### Instances
``` purescript
(BytesPerValue Int8 b, Nat b) => DataView Int8 LE Int
(BytesPerValue Int16 b, Nat b) => DataView Int16 LE Int
(BytesPerValue Int32 b, Nat b) => DataView Int32 LE Int
(BytesPerValue Uint8 b, Nat b) => DataView Uint8 LE UInt
(BytesPerValue Uint16 b, Nat b) => DataView Uint16 LE UInt
(BytesPerValue Uint32 b, Nat b) => DataView Uint32 LE UInt
(BytesPerValue Float32 b, Nat b) => DataView Float32 LE Number
(BytesPerValue Float64 b, Nat b) => DataView Float64 LE Number
```

#### `DataView`

``` purescript
class (BinaryValue a t) <= DataView (a :: ArrayViewType) (e :: Endianness) t | a -> t where
  get :: DVProxy a e -> DataView -> ByteOffset -> Effect (Maybe t)
  set :: DVProxy a e -> DataView -> t -> ByteOffset -> Effect Unit
```

##### Instances
``` purescript
(BytesPerValue Int8 b, Nat b) => DataView Int8 BE Int
(BytesPerValue Int8 b, Nat b) => DataView Int8 LE Int
(BytesPerValue Int16 b, Nat b) => DataView Int16 BE Int
(BytesPerValue Int16 b, Nat b) => DataView Int16 LE Int
(BytesPerValue Int32 b, Nat b) => DataView Int32 BE Int
(BytesPerValue Int32 b, Nat b) => DataView Int32 LE Int
(BytesPerValue Uint8 b, Nat b) => DataView Uint8 BE UInt
(BytesPerValue Uint8 b, Nat b) => DataView Uint8 LE UInt
(BytesPerValue Uint16 b, Nat b) => DataView Uint16 BE UInt
(BytesPerValue Uint16 b, Nat b) => DataView Uint16 LE UInt
(BytesPerValue Uint32 b, Nat b) => DataView Uint32 BE UInt
(BytesPerValue Uint32 b, Nat b) => DataView Uint32 LE UInt
(BytesPerValue Float32 b, Nat b) => DataView Float32 BE Number
(BytesPerValue Float32 b, Nat b) => DataView Float32 LE Number
(BytesPerValue Float64 b, Nat b) => DataView Float64 BE Number
(BytesPerValue Float64 b, Nat b) => DataView Float64 LE Number
```


