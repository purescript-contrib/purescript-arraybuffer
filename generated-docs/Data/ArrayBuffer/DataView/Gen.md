## Module Data.ArrayBuffer.DataView.Gen

#### `genDataView`

``` purescript
genDataView :: forall m. MonadGen m => ByteLength -> Maybe ByteLength -> m DataView
```

#### `WithOffsetAndValue`

``` purescript
data WithOffsetAndValue n (a :: ArrayViewType) (e :: Endianness) t
  = WithOffsetAndValue (Vec n ByteOffset) t DataView
```

For generating some set of offsets residing inside the generated array, with some computable value

#### `genWithOffsetAndValue`

``` purescript
genWithOffsetAndValue :: forall m n a b e t. MonadGen m => Nat n => BytesPerValue a b => DataView a e t => Nat b => m DataView -> m t -> m (WithOffsetAndValue n a e t)
```


