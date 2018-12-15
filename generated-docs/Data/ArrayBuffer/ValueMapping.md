## Module Data.ArrayBuffer.ValueMapping

This module represents type-level mappings between `ArrayViewType`s
and meaningful data.

#### `BytesPerValue`

``` purescript
class BytesPerValue (a :: ArrayViewType) (b :: Type) | a -> b
```

Maps a `TypedArray`'s binary casted value, to the space occupied by that value, in bytes.

##### Instances
``` purescript
BytesPerValue Uint8Clamped D1
BytesPerValue Uint32 D4
BytesPerValue Uint16 D2
BytesPerValue Uint8 D1
BytesPerValue Int32 D4
BytesPerValue Int16 D2
BytesPerValue Int8 D1
BytesPerValue Float32 D4
BytesPerValue Float64 D8
```

#### `BinaryValue`

``` purescript
class BinaryValue (a :: ArrayViewType) (t :: Type) | a -> t
```

Maps a `TypedArray`'s binary casted value, to its computable representation in JavaScript.

##### Instances
``` purescript
BinaryValue Uint8Clamped UInt
BinaryValue Uint32 UInt
BinaryValue Uint16 UInt
BinaryValue Uint8 UInt
BinaryValue Int32 Int
BinaryValue Int16 Int
BinaryValue Int8 Int
BinaryValue Float32 Number
BinaryValue Float64 Number
```


