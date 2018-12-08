## Module Data.ArrayBuffer.ArrayBuffer

This module represents the functional bindings to JavaScript's `ArrayBuffer`
objects. See [MDN's spec](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/ArrayBuffer) for details.

#### `empty`

``` purescript
empty :: ByteLength -> ArrayBuffer
```

Create an `ArrayBuffer` with the given capacity.

#### `byteLength`

``` purescript
byteLength :: ArrayBuffer -> ByteLength
```

Represents the length of an `ArrayBuffer` in bytes.

#### `slice`

``` purescript
slice :: ArrayBuffer -> Maybe (Tuple ByteOffset (Maybe ByteOffset)) -> ArrayBuffer
```

Returns a new `ArrayBuffer` whose contents are a copy of this ArrayBuffer's bytes from begin, inclusive, up to end, exclusive.


