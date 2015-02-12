
module Data.ArrayBuffer.Typed where

import Data.ArrayBuffer
import Data.ArrayBuffer.DataView(DataView())
import Data.Function
import Data.Maybe
import Data.ArrayBuffer.Types

foreign import data ArrayView :: * -> *

type Int8Array = ArrayView Int8
type Uint8Array = ArrayView Uint8
--type Uint8ClampedArray = ArrayView Uint8Clamped
--type Int16Array = ArrayView Int16
--type Uint16Array = ArrayView Uint16
--type Int32Array = ArrayView Int32
--type Uint32Array = ArrayView Uint32
--type Float32Array = ArrayView Float32
--type Float64Array = ArrayView Float64 

foreign import asInt8Array
"""
function asInt8Array(v) {
  return new Int8Array(v.buffer, v.byteOffset, v.byteLength);
}
""":: DataView -> Int8Array

foreign import asUint8Array
"""
function asUint8Array(v) {
  return new Uint8Array(v.buffer, v.byteOffset, v.byteLength);
}
""":: DataView -> Uint8Array

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
  var ret = [];
  for (var i = 0, l = a.length; i < l; i++)
    ret[i] = a[i];
  return ret;
}
""" :: forall a. ArrayView a -> [Number]
