## Module Data.ArrayBuffer.Typed.Gen

Functions for generating typed arrays and values.

#### `genUint8ClampedArray`

``` purescript
genUint8ClampedArray :: forall m. MonadGen m => m Uint8ClampedArray
```

#### `genUint32Array`

``` purescript
genUint32Array :: forall m. MonadGen m => m Uint32Array
```

#### `genUint16Array`

``` purescript
genUint16Array :: forall m. MonadGen m => m Uint16Array
```

#### `genUint8Array`

``` purescript
genUint8Array :: forall m. MonadGen m => m Uint8Array
```

#### `genInt32Array`

``` purescript
genInt32Array :: forall m. MonadGen m => m Int32Array
```

#### `genInt16Array`

``` purescript
genInt16Array :: forall m. MonadGen m => m Int16Array
```

#### `genInt8Array`

``` purescript
genInt8Array :: forall m. MonadGen m => m Int8Array
```

#### `genFloat32Array`

``` purescript
genFloat32Array :: forall m. MonadGen m => m Float32Array
```

#### `genFloat64Array`

``` purescript
genFloat64Array :: forall m. MonadGen m => m Float64Array
```

#### `genUByte`

``` purescript
genUByte :: forall m. MonadGen m => m Int
```

#### `genByte`

``` purescript
genByte :: forall m. MonadGen m => m Int
```

#### `genUChomp`

``` purescript
genUChomp :: forall m. MonadGen m => m Int
```

#### `genChomp`

``` purescript
genChomp :: forall m. MonadGen m => m Int
```

#### `genUWord`

``` purescript
genUWord :: forall m. MonadGen m => m UInt
```

#### `genWord`

``` purescript
genWord :: forall m. MonadGen m => m Int
```

#### `genFloat32`

``` purescript
genFloat32 :: forall m. MonadGen m => m Number
```

#### `genFloat64`

``` purescript
genFloat64 :: forall m. MonadGen m => m Number
```

#### `WithOffset`

``` purescript
data WithOffset n a
  = WithOffset (Vec n Offset) (ArrayView a)
```

For generating some set of offsets residing inside the generated array

##### Instances
``` purescript
(Generic (ArrayView a) a') => Generic (WithOffset n a) _
```

#### `genWithOffset`

``` purescript
genWithOffset :: forall m n b a. MonadGen m => Nat n => BytesPerValue a b => m (ArrayView a) -> m (WithOffset n a)
```


