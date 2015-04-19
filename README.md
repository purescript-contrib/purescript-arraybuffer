# Module Documentation

## Module Data.ArrayBuffer.ArrayBuffer

#### `create`

``` purescript
create :: ByteLength -> ArrayBuffer
```


#### `byteLength`

``` purescript
byteLength :: ArrayBuffer -> ByteLength
```


#### `sliceImpl`

``` purescript
sliceImpl :: Fn3 ByteOffset ByteOffset ArrayBuffer ArrayBuffer
```


#### `slice`

``` purescript
slice :: ByteOffset -> ByteOffset -> ArrayBuffer -> ArrayBuffer
```


#### `fromArray`

``` purescript
fromArray :: [Number] -> ArrayBuffer
```


#### `fromString`

``` purescript
fromString :: String -> ArrayBuffer
```



## Module Data.ArrayBuffer.DataView

#### `whole`

``` purescript
whole :: ArrayBuffer -> DataView
```


#### `sliceImpl`

``` purescript
sliceImpl :: forall e. Fn5 (DataView -> Maybe DataView) (Maybe DataView) ByteOffset ByteLength ArrayBuffer (Maybe DataView)
```


#### `slice`

``` purescript
slice :: forall e. ByteOffset -> ByteLength -> ArrayBuffer -> Maybe DataView
```


#### `buffer`

``` purescript
buffer :: DataView -> ArrayBuffer
```


#### `byteOffset`

``` purescript
byteOffset :: DataView -> ByteOffset
```


#### `byteLength`

``` purescript
byteLength :: DataView -> ByteLength
```


#### `Reader`

``` purescript
data Reader :: !
```


#### `getterImpl`

``` purescript
getterImpl :: forall e r. Fn6 (r -> Maybe r) (Maybe r) String ByteLength DataView ByteOffset (Eff (reader :: Reader | e) (Maybe r))
```


#### `getter`

``` purescript
getter :: forall e r. String -> ByteLength -> DataView -> ByteOffset -> Eff (reader :: Reader | e) (Maybe r)
```


#### `Getter`

``` purescript
type Getter r = forall e. DataView -> ByteOffset -> Eff (reader :: Reader | e) (Maybe r)
```


#### `Writer`

``` purescript
data Writer :: !
```


#### `setter`

``` purescript
setter :: forall e r. String -> DataView -> r -> ByteOffset -> Eff (writer :: Writer | e) Unit
```


#### `Setter`

``` purescript
type Setter r = forall e. DataView -> r -> ByteOffset -> Eff (writer :: Writer | e) Unit
```


#### `getInt8`

``` purescript
getInt8 :: Getter Number
```


#### `getInt16`

``` purescript
getInt16 :: Getter Number
```


#### `getInt32`

``` purescript
getInt32 :: Getter Number
```


#### `getUint8`

``` purescript
getUint8 :: Getter Number
```


#### `getUint16`

``` purescript
getUint16 :: Getter Number
```


#### `getUint32`

``` purescript
getUint32 :: Getter Number
```


#### `getFloat32`

``` purescript
getFloat32 :: Getter Number
```


#### `getFloat64`

``` purescript
getFloat64 :: Getter Number
```


#### `setInt8`

``` purescript
setInt8 :: Setter Number
```


#### `setInt16`

``` purescript
setInt16 :: Setter Number
```


#### `setInt32`

``` purescript
setInt32 :: Setter Number
```


#### `setUint8`

``` purescript
setUint8 :: Setter Number
```


#### `setUint16`

``` purescript
setUint16 :: Setter Number
```


#### `setUint32`

``` purescript
setUint32 :: Setter Number
```


#### `setFloat32`

``` purescript
setFloat32 :: Setter Number
```


#### `setFloat64`

``` purescript
setFloat64 :: Setter Number
```



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


#### `asInt16Array`

``` purescript
asInt16Array :: DataView -> Int16Array
```


#### `asInt32Array`

``` purescript
asInt32Array :: DataView -> Int32Array
```


#### `asUint8Array`

``` purescript
asUint8Array :: DataView -> Uint8Array
```


#### `asUint16Array`

``` purescript
asUint16Array :: DataView -> Uint16Array
```


#### `asUint32Array`

``` purescript
asUint32Array :: DataView -> Uint32Array
```


#### `asUint8ClampedArray`

``` purescript
asUint8ClampedArray :: DataView -> Uint8ClampedArray
```


#### `asFloat32Array`

``` purescript
asFloat32Array :: DataView -> Float32Array
```


#### `asFloat64Array`

``` purescript
asFloat64Array :: DataView -> Float64Array
```


#### `dataView`

``` purescript
dataView :: forall a. ArrayView a -> DataView
```


#### `setImpl`

``` purescript
setImpl :: forall a e. Fn3 (ArrayView a) ByteOffset (ArrayView a) (Eff (writer :: Writer | e) Unit)
```


#### `set`

``` purescript
set :: forall a e. ArrayView a -> ByteOffset -> ArrayView a -> Eff (writer :: Writer | e) Unit
```


#### `unsafeAtImpl`

``` purescript
unsafeAtImpl :: forall a. Fn2 (ArrayView a) Number Number
```


#### `hasIndexImpl`

``` purescript
hasIndexImpl :: forall a. Fn2 (ArrayView a) Number Boolean
```


#### `at`

``` purescript
at :: forall a. ArrayView a -> Number -> Maybe Number
```


#### `toArray`

``` purescript
toArray :: forall a. ArrayView a -> [Number]
```