# Module Documentation

## Module Data.ArrayBuffer.ArrayBuffer

#### `create`

``` purescript
create :: ByteLength -> ArrayBuffer
```

Create an `ArrayBuffer` with the given capacity.

#### `byteLength`

``` purescript
byteLength :: ArrayBuffer -> ByteLength
```

Represents the length of an `ArrayBuffer` in bytes.

#### `sliceImpl`

``` purescript
sliceImpl :: Fn3 ByteOffset ByteOffset ArrayBuffer ArrayBuffer
```


#### `slice`

``` purescript
slice :: ByteOffset -> ByteOffset -> ArrayBuffer -> ArrayBuffer
```

Returns a new `ArrayBuffer` whose contents are a copy of this ArrayBuffer's bytes from begin, inclusive, up to end, exclusive.

#### `fromArray`

``` purescript
fromArray :: [Number] -> ArrayBuffer
```

Convert an array into an `ArrayBuffer` representation.

#### `fromString`

``` purescript
fromString :: String -> ArrayBuffer
```

Convert a string into an `ArrayBuffer` representation.


## Module Data.ArrayBuffer.DataView

#### `Getter`

``` purescript
type Getter r = forall e. DataView -> ByteOffset -> Eff (reader :: Reader | e) (Maybe r)
```

Type for all fetching functions.

#### `Setter`

``` purescript
type Setter r = forall e. DataView -> r -> ByteOffset -> Eff (writer :: Writer | e) Unit
```

Type for all storing functions.

#### `whole`

``` purescript
whole :: ArrayBuffer -> DataView
```

View mapping the whole `ArrayBuffer`.

#### `slice`

``` purescript
slice :: forall e. ByteOffset -> ByteLength -> ArrayBuffer -> Maybe DataView
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

#### `Reader`

``` purescript
data Reader :: !
```


#### `Writer`

``` purescript
data Writer :: !
```


#### `getInt8`

``` purescript
getInt8 :: Getter Number
```

Fetch int8 value at a certain index in a `DataView`.

#### `getInt16`

``` purescript
getInt16 :: Getter Number
```

Fetch int16 value at a certain index in a `DataView`.

#### `getInt32`

``` purescript
getInt32 :: Getter Number
```

Fetch int32 value at a certain index in a `DataView`.

#### `getUint8`

``` purescript
getUint8 :: Getter Number
```

Fetch uint8 value at a certain index in a `DataView`.

#### `getUint16`

``` purescript
getUint16 :: Getter Number
```

Fetch uint16 value at a certain index in a `DataView`.

#### `getUint32`

``` purescript
getUint32 :: Getter Number
```

Fetch uint32 value at a certain index in a `DataView`.

#### `getFloat32`

``` purescript
getFloat32 :: Getter Number
```

Fetch float32 value at a certain index in a `DataView`.

#### `getFloat64`

``` purescript
getFloat64 :: Getter Number
```

Fetch float64 value at a certain index in a `DataView`.

#### `setInt8`

``` purescript
setInt8 :: Setter Number
```

Store int8 value at a certain index in a `DataView`.

#### `setInt16`

``` purescript
setInt16 :: Setter Number
```

Store int16 value at a certain index in a `DataView`.

#### `setInt32`

``` purescript
setInt32 :: Setter Number
```

Store int32 value at a certain index in a `DataView`.

#### `setUint8`

``` purescript
setUint8 :: Setter Number
```

Store uint8 value at a certain index in a `DataView`.

#### `setUint16`

``` purescript
setUint16 :: Setter Number
```

Store uint16 value at a certain index in a `DataView`.

#### `setUint32`

``` purescript
setUint32 :: Setter Number
```

Store uint32 value at a certain index in a `DataView`.

#### `setFloat32`

``` purescript
setFloat32 :: Setter Number
```

Store float32 value at a certain index in a `DataView`.

#### `setFloat64`

``` purescript
setFloat64 :: Setter Number
```

Store float64 value at a certain index in a `DataView`.


## Module Data.ArrayBuffer.Show

#### `showArrayView`

``` purescript
instance showArrayView :: Show (ArrayView a)
```


#### `showDataView`

``` purescript
instance showDataView :: Show DataView
```


#### `showArrayBuffer`

``` purescript
instance showArrayBuffer :: Show ArrayBuffer
```


#### `showImpl`

``` purescript
showImpl :: forall a. ArrayView a -> String
```



## Module Data.ArrayBuffer.Typed

#### `Writer`

``` purescript
data Writer :: !
```


#### `asInt8Array`

``` purescript
asInt8Array :: DataView -> Int8Array
```

Create typed int8 array viewing the buffer mapped by the `DataView`

#### `asInt16Array`

``` purescript
asInt16Array :: DataView -> Int16Array
```

Create typed int16 array viewing the buffer mapped by the `DataView`

#### `asInt32Array`

``` purescript
asInt32Array :: DataView -> Int32Array
```

Create typed int32 array viewing the buffer mapped by the `DataView`

#### `asUint8Array`

``` purescript
asUint8Array :: DataView -> Uint8Array
```

Create typed uint8 array viewing the buffer mapped by the `DataView`

#### `asUint16Array`

``` purescript
asUint16Array :: DataView -> Uint16Array
```

Create typed uint16 array viewing the buffer mapped by the `DataView`

#### `asUint32Array`

``` purescript
asUint32Array :: DataView -> Uint32Array
```

Create typed uint32 array viewing the buffer mapped by the `DataView`

#### `asUint8ClampedArray`

``` purescript
asUint8ClampedArray :: DataView -> Uint8ClampedArray
```

Create typed uint8 clamped array viewing the buffer mapped by the `DataView`

#### `asFloat32Array`

``` purescript
asFloat32Array :: DataView -> Float32Array
```

Create typed float32 array viewing the buffer mapped by the `DataView`

#### `asFloat64Array`

``` purescript
asFloat64Array :: DataView -> Float64Array
```

Create typed float64 array viewing the buffer mapped by the `DataView`

#### `dataView`

``` purescript
dataView :: forall a. ArrayView a -> DataView
```

Interpret typed array as a `DataView`.

#### `set`

``` purescript
set :: forall a e. ArrayView a -> ByteOffset -> ArrayView a -> Eff (writer :: Writer | e) Unit
```

Stores multiple values in the last typed array, reading input values from ther first typed array.

#### `at`

``` purescript
at :: forall a. ArrayView a -> Number -> Maybe Number
```

Fetch element at index.

#### `toArray`

``` purescript
toArray :: forall a. ArrayView a -> [Number]
```

Turn typed array into an array.



