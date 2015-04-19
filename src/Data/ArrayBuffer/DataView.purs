module Data.ArrayBuffer.DataView where

import Data.ArrayBuffer.Types
import Data.Function
import Data.Maybe
import Control.Monad.Eff

foreign import whole
"""
function whole(b) {
  return new DataView(b);
}
""" :: ArrayBuffer -> DataView

foreign import sliceImpl
"""
function sliceImpl(just, nothing, s, l, b) {
  return s + l <= b.byteLength? just(new DataView(b, s, l)) : nothing;
}
""" :: forall e. Fn5 (DataView -> Maybe DataView) (Maybe DataView) ByteOffset ByteLength ArrayBuffer (Maybe DataView)
slice :: forall e. ByteOffset -> ByteLength -> ArrayBuffer -> Maybe DataView
slice = runFn5 sliceImpl Just Nothing

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

foreign import getterImpl
"""
function getterImpl(just, nothing, s, l, v, o) {
  return function() {
    return (o + l) <= v.byteLength? just(v[s].call(v,o)) : nothing;
  };
}
""" :: forall e r. Fn6 (r -> Maybe r) (Maybe r) String ByteLength DataView ByteOffset (Eff (reader :: Reader | e) (Maybe r))

getter :: forall e r. String -> ByteLength -> DataView -> ByteOffset -> Eff (reader :: Reader | e) (Maybe r)
getter = runFn6 getterImpl Just Nothing

type Getter r = forall e. DataView -> ByteOffset -> Eff (reader :: Reader | e) (Maybe r)

foreign import data Writer :: !

foreign import setter
"""
function setter(s) {
  return function(v) {
    var f = v[s];
    return function(n) {
      return function(o) {
        return function() {
          f.call(v,o,n);
        };
      };
    };
  };
}
""" :: forall e r. String -> DataView -> r -> ByteOffset -> Eff (writer :: Writer | e) Unit

type Setter r = forall e. DataView -> r -> ByteOffset -> Eff (writer :: Writer | e) Unit

getInt8 :: Getter Number
getInt8 = getter "getInt8" 1

getInt16 :: Getter Number
getInt16 = getter "getInt16" 2

getInt32 :: Getter Number
getInt32 = getter "getInt32" 4

getUint8 :: Getter Number
getUint8 = getter "getUint8" 1

getUint16 :: Getter Number
getUint16 = getter "getUint16" 2

getUint32 :: Getter Number
getUint32 = getter "getUint32" 4

getFloat32 :: Getter Number
getFloat32 = getter "getFloat32" 4

getFloat64 :: Getter Number
getFloat64 = getter "getFloat64" 8


setInt8 :: Setter Number
setInt8 = setter "setInt8"

setInt16 :: Setter Number
setInt16 = setter "setInt16"

setInt32 :: Setter Number
setInt32 = setter "setInt32"

setUint8 :: Setter Number
setUint8 = setter "setUint8"

setUint16 :: Setter Number
setUint16 = setter "setUint16"

setUint32 :: Setter Number
setUint32 = setter "setUint32"

setFloat32 :: Setter Number
setFloat32 = setter "setFloat32"

setFloat64 :: Setter Number
setFloat64 = setter "setFloat64"


