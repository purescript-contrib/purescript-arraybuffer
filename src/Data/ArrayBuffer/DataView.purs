module Data.ArrayBuffer.DataView where

import Data.ArrayBuffer(ArrayBuffer())
import Data.ArrayBuffer.Types
import Data.Function
import Control.Monad.Eff

foreign import data DataView :: *

foreign import whole
"""
function whole(b) {
  return new DataView(b);
}
""" :: ArrayBuffer -> DataView

foreign import sliceImpl
"""
function sliceImpl(s, l, b) {
  return new DataView(b, s, l);
}
""" :: forall e. Fn3 ByteOffset ByteLength ArrayBuffer DataView
slice :: forall e. ByteOffset -> ByteLength -> ArrayBuffer -> DataView
slice = runFn3 sliceImpl

foreign import buffer
"""
function buffer(v) {
  return v.buffer;
}
""" :: DataView -> ArrayBuffer

foreign import byteOffset
"""
function byteOffset(v) {
  return v.byteOffset;
}
""" :: DataView -> ByteOffset

foreign import byteLength
"""
function byteLength(v) {
  return v.byteLength;
}
""" :: DataView -> ByteLength


foreign import data Reader :: !

foreign import getter
"""
function getter(s) {
  return function(v) {
    var f = v[s];
    return function(o) {
      return function() {
        return f.call(v,o);
      };
    };
  };
}
""" :: forall e r. String -> DataView -> ByteOffset -> Eff (reader :: Reader | e) r

type Getter r = forall e. DataView -> ByteOffset -> Eff (reader :: Reader | e) r

foreign import data Writer :: !

foreign import setter
"""
function setter(s) {
  return function(v) {
    var f = v[s];
    return function(n) {
      return function(o) {
        return function() {
          return f.call(v,o,n);
        };
      };
    };
  };
}
""" :: forall e r. String -> DataView -> r -> ByteOffset -> Eff (writer :: Writer | e) Unit

type Setter r = forall e. DataView -> r -> ByteOffset -> Eff (writer :: Writer | e) Unit


getInt8 :: Getter Int8
getInt8 = getter "getInt8"

getInt16 :: Getter Int16
getInt16 = getter "getInt16"

getInt32 :: Getter Int32
getInt32 = getter "getInt32"

getUint8 :: Getter Uint8
getUint8 = getter "getUint8"

getUint16 :: Getter Uint16
getUint16 = getter "getUint16"

getUint32 :: Getter Uint32
getUint32 = getter "getUint32"

getUint8Clamped :: Getter Uint8Clamped
getUint8Clamped = getter "getUint8Clamped"

getFloat32 :: Getter Float32
getFloat32 = getter "getFloat32"

getFloat64 :: Getter Float64
getFloat64 = getter "getFloat64"


setInt8 :: Setter Int8
setInt8 = setter "setInt8"

setInt16 :: Setter Int16
setInt16 = setter "setInt16"

setInt32 :: Setter Int32
setInt32 = setter "setInt32"

setUint8 :: Setter Uint8
setUint8 = setter "setUint8"

setUint16 :: Setter Uint16
setUint16 = setter "setUint16"

setUint32 :: Setter Uint32
setUint32 = setter "setUint32"

setUint8Clamped :: Setter Uint8Clamped
setUint8Clamped = setter "setUint8Clamped"

setFloat32 :: Setter Float32
setFloat32 = setter "setFloat32"

setFloat64 :: Setter Float64
setFloat64 = setter "setFloat64"


