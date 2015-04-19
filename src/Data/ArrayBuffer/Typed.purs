module Data.ArrayBuffer.Typed( Writer()
                             , asInt8Array
                             , asInt16Array
                             , asInt32Array
                             , asUint8Array
                             , asUint16Array
                             , asUint32Array
                             , asUint8ClampedArray
                             , asFloat32Array
                             , asFloat64Array
                             , dataView
                             , set
                             , unsafeAt
                             , hasIndex
                             , at
                             , toArray
                             ) where

import Data.ArrayBuffer.Types
import Data.ArrayBuffer.ArrayBuffer
import Data.Function
import Data.Maybe
import Control.Monad.Eff

foreign import data Writer :: !

-- | Create typed int8 array viewing the buffer mapped by the `DataView`
foreign import asInt8Array
"""
function asInt8Array(v) {
  return new Int8Array(v.buffer, v.byteOffset, v.byteLength);
}
""":: DataView -> Int8Array


-- | Create typed int16 array viewing the buffer mapped by the `DataView`
foreign import asInt16Array
"""
function asInt16Array(v) {
  return new Int16Array(v.buffer, v.byteOffset, v.byteLength >>> 1);
}
""":: DataView -> Int16Array

-- | Create typed int32 array viewing the buffer mapped by the `DataView`
foreign import asInt32Array
"""
function asInt32Array(v) {
  return new Int32Array(v.buffer, v.byteOffset, v.byteLength >>> 2);
}
""":: DataView -> Int32Array

-- | Create typed uint8 array viewing the buffer mapped by the `DataView`
foreign import asUint8Array
"""
function asUint8Array(v) {
  return new Uint8Array(v.buffer, v.byteOffset, v.byteLength);
}
""":: DataView -> Uint8Array

-- | Create typed uint16 array viewing the buffer mapped by the `DataView`
foreign import asUint16Array
"""
function asUint16Array(v) {
  return new Uint16Array(v.buffer, v.byteOffset, v.byteLength >>> 1);
}
""":: DataView -> Uint16Array

-- | Create typed uint32 array viewing the buffer mapped by the `DataView`
foreign import asUint32Array
"""
function asUint32Array(v) {
  return new Uint32Array(v.buffer, v.byteOffset, v.byteLength >>> 2);
}
""":: DataView -> Uint32Array

-- | Create typed uint8 clamped array viewing the buffer mapped by the `DataView`
foreign import asUint8ClampedArray
"""
function asUint8ClampedArray(v) {
  return new Uint8ClampedArray(v.buffer, v.byteOffset, v.byteLength);
}
""":: DataView -> Uint8ClampedArray

-- | Create typed float32 array viewing the buffer mapped by the `DataView`
foreign import asFloat32Array
"""
function asFloat32Array(v) {
  return new Float32Array(v.buffer, v.byteOffset, v.byteLength >>> 2);
}
""":: DataView -> Float32Array

-- | Create typed float64 array viewing the buffer mapped by the `DataView`
foreign import asFloat64Array
"""
function asFloat64Array(v) {
  return new Float64Array(v.buffer, v.byteOffset, v.byteLength >>> 3);
}
""":: DataView -> Float64Array

-- | Interpret typed array as a `DataView`.
foreign import dataView
"""
function dataView(a) {
  return a;
}
""" :: forall a. ArrayView a -> DataView

foreign import setImpl
"""
function setImpl(ra, off, a) {
  return function() {
    a.set(ra, off);
  };
}""" :: forall a e. Fn3 (ArrayView a) ByteOffset (ArrayView a) (Eff (writer :: Writer | e) Unit)

-- | Stores multiple values in the last typed array, reading input values from ther first typed array.
set :: forall a e. ArrayView a -> ByteOffset -> ArrayView a -> Eff (writer :: Writer | e) Unit
set = runFn3 setImpl

foreign import unsafeAtImpl
"""
function unsafeAtImpl(a, i) {
   return a[i];
}
""" :: forall a. Fn2 (ArrayView a) Number Number

-- | Fetch element at index.
unsafeAt = runFn2 unsafeAtImpl

foreign import hasIndexImpl
"""
function hasIndexImpl(a, i) {
  return i in a;
}
""" :: forall a. Fn2 (ArrayView a) Number Boolean

-- | Determine if a certain index is valid.
hasIndex = runFn2 hasIndexImpl

-- | Fetch element at index.
at :: forall a. ArrayView a -> Number -> Maybe Number
at a n = if a `hasIndex` n then
           Just $ unsafeAt a n
         else
           Nothing

-- | Turn typed array into an array.
foreign import toArray
"""
function toArray(a) {
  var l = a.length;
  var ret = new Array(l);
  for (var i = 0; i < l; i++)
    ret[i] = a[i];
  return ret;
}
""" :: forall a. ArrayView a -> [Number]
