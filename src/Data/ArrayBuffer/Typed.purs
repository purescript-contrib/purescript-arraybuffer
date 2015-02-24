
module Data.ArrayBuffer.Typed where

import Data.ArrayBuffer.Types
import Data.ArrayBuffer.ArrayBuffer
import Data.Function
import Data.Maybe

foreign import asInt8Array
"""
function asInt8Array(v) {
  return new Int8Array(v.buffer, v.byteOffset, v.byteLength);
}
""":: DataView -> Int8Array

foreign import asInt16Array
"""
function asInt16Array(v) {
  return new Int16Array(v.buffer, v.byteOffset, v.byteLength >>> 1);
}
""":: DataView -> Int16Array

foreign import asInt32Array
"""
function asInt32Array(v) {
  return new Int32Array(v.buffer, v.byteOffset, v.byteLength >>> 2);
}
""":: DataView -> Int32Array

foreign import asUint8Array
"""
function asUint8Array(v) {
  return new Uint8Array(v.buffer, v.byteOffset, v.byteLength);
}
""":: DataView -> Uint8Array

foreign import asUint16Array
"""
function asUint16Array(v) {
  return new Uint16Array(v.buffer, v.byteOffset, v.byteLength >>> 1);
}
""":: DataView -> Uint16Array

foreign import asUint32Array
"""
function asUint32Array(v) {
  return new Uint32Array(v.buffer, v.byteOffset, v.byteLength >>> 2);
}
""":: DataView -> Uint32Array

foreign import asUint8ClampedArray
"""
function asUint8ClampedArray(v) {
  return new Uint8ClampedArray(v.buffer, v.byteOffset, v.byteLength);
}
""":: DataView -> Uint8ClampedArray

foreign import asFloat32Array
"""
function asFloat32Array(v) {
  return new Float32Array(v.buffer, v.byteOffset, v.byteLength >>> 2);
}
""":: DataView -> Float32Array

foreign import asFloat64Array
"""
function asFloat64Array(v) {
  return new Float64Array(v.buffer, v.byteOffset, v.byteLength >>> 3);
}
""":: DataView -> Float64Array

foreign import dataView
"""
function dataView(a) {
  return a;
}
""" :: forall a. ArrayView a -> DataView

foreign import unsafeAtImpl
"""
function unsafeAtImpl(a, i) {
   return a[i];
}
""" :: forall a. Fn2 (ArrayView a) Number Number
unsafeAt = runFn2 unsafeAtImpl

foreign import hasIndexImpl
"""
function hasIndexImpl(a, i) {
  return i in a;
}
""" :: forall a. Fn2 (ArrayView a) Number Boolean
hasIndex = runFn2 hasIndexImpl

at :: forall a. ArrayView a -> Number -> Maybe Number
at a n = if a `hasIndex` n then
             Just $ unsafeAt a n
           else
             Nothing


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
