## Module Data.ArrayBuffer.Typed.Gen

Functions for generating typed arrays and values.

#### `genTypedArray`

``` purescript
genTypedArray :: forall m a t. MonadGen m => TypedArray a t => Length -> Maybe Length -> m t -> m (ArrayView a)
```

#### `genUByte`

``` purescript
genUByte :: forall m. MonadGen m => m UInt
```

#### `genByte`

``` purescript
genByte :: forall m. MonadGen m => m Int
```

#### `genUChomp`

``` purescript
genUChomp :: forall m. MonadGen m => m UInt
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


