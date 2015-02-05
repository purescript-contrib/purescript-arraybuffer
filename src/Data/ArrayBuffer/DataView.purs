module Data.ArrayBuffer.DataView where

import Data.ArrayBuffer(ArrayBuffer(), ByteOffset(), ByteLength())
import Data.ArrayBuffer.Types
import Data.Function
import Control.Monad.Eff

foreign import data DataView :: *


foreign import whole """
function whole(b) {
  return new DataView(b);
}
""" :: ArrayBuffer -> DataView

foreign import sliceImpl """
function sliceImpl(s, l, b) {
  return new DataView(b, s, l);
}
""" :: forall e. Fn3 ByteOffset ByteLength ArrayBuffer DataView
slice :: forall e. ByteOffset -> ByteLength -> ArrayBuffer -> DataView
slice = runFn3 sliceImpl

foreign import buffer """
function buffer(v) {
  return v.buffer;
}
""" :: DataView -> ArrayBuffer

foreign import byteOffset """
function byteOffset(v) {
  return v.byteOffset;
}
""" :: DataView -> ByteOffset

foreign import byteLength """
function byteLength(v) {
  return v.byteLength;
}
""" :: DataView -> ByteLength


foreign import data Reader :: !                    

foreign import getInt8 """
function getInt8(v) {
  return function(o) {
    return function() {
      return v.getInt8(o);
    };
  };
}
""" :: forall e. DataView -> ByteOffset -> Eff (reader :: Reader | e) Int8

foreign import data Writer :: !

foreign import setInt8 """
function setInt8(v) {
  return function(o) {
    return function(n) {
      return function() {
        v.setInt8(o, n);
      };
    };
  };
}
""" :: forall e. DataView -> ByteOffset -> Int8 -> Eff (writer :: Writer | e) Unit

