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

#### `Getter`

``` purescript
type Getter r = DataView -> ByteOffset -> Effect (Maybe r)
```

Type for all fetching functions.

#### `getInt8`

``` purescript
getInt8 :: Getter Int
```

Fetch int8 value at a certain index in a `DataView`.

#### `getInt16be`

``` purescript
getInt16be :: Getter Int
```

Fetch int16 value at a certain index in a `DataView`.

#### `getInt32be`

``` purescript
getInt32be :: Getter Int
```

Fetch int32 value at a certain index in a `DataView`.

#### `getUint8`

``` purescript
getUint8 :: Getter Int
```

Fetch uint8 value at a certain index in a `DataView`.

#### `getUint16be`

``` purescript
getUint16be :: Getter Int
```

Fetch uint16 value at a certain index in a `DataView`.

#### `getUint32be`

``` purescript
getUint32be :: Getter Number
```

Fetch uint32 value at a certain index in a `DataView`.

#### `getFloat32be`

``` purescript
getFloat32be :: Getter Number
```

Fetch float32 value at a certain index in a `DataView`.

#### `getFloat64be`

``` purescript
getFloat64be :: Getter Number
```

Fetch float64 value at a certain index in a `DataView`.

#### `getInt16le`

``` purescript
getInt16le :: Getter Int
```

#### `getInt32le`

``` purescript
getInt32le :: Getter Int
```

#### `getUint16le`

``` purescript
getUint16le :: Getter Int
```

#### `getUint32le`

``` purescript
getUint32le :: Getter Number
```

#### `getFloat32le`

``` purescript
getFloat32le :: Getter Number
```

#### `getFloat64le`

``` purescript
getFloat64le :: Getter Number
```

#### `Setter`

``` purescript
type Setter r = DataView -> r -> ByteOffset -> Effect Unit
```

Type for all storing functions.

#### `setInt8`

``` purescript
setInt8 :: Setter Int
```

Store int8 value at a certain index in a `DataView`.

#### `setInt16be`

``` purescript
setInt16be :: Setter Int
```

Store int16 value at a certain index in a `DataView`.

#### `setInt32be`

``` purescript
setInt32be :: Setter Int
```

Store int32 value at a certain index in a `DataView`.

#### `setUint8`

``` purescript
setUint8 :: Setter Int
```

Store uint8 value at a certain index in a `DataView`.

#### `setUint16be`

``` purescript
setUint16be :: Setter Int
```

Store uint16 value at a certain index in a `DataView`.

#### `setUint32be`

``` purescript
setUint32be :: Setter Number
```

Store uint32 value at a certain index in a `DataView`.

#### `setFloat32be`

``` purescript
setFloat32be :: Setter Number
```

Store float32 value at a certain index in a `DataView`.

#### `setFloat64be`

``` purescript
setFloat64be :: Setter Number
```

Store float64 value at a certain index in a `DataView`.

#### `setInt16le`

``` purescript
setInt16le :: Setter Int
```

#### `setInt32le`

``` purescript
setInt32le :: Setter Int
```

#### `setUint16le`

``` purescript
setUint16le :: Setter Int
```

#### `setUint32le`

``` purescript
setUint32le :: Setter Number
```

#### `setFloat32le`

``` purescript
setFloat32le :: Setter Number
```

#### `setFloat64le`

``` purescript
setFloat64le :: Setter Number
```


