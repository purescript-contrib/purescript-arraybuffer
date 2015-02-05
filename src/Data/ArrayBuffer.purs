module Data.ArrayBuffer where

import Data.Function

foreign import data ArrayBuffer :: *

type ByteOffset = Number
type ByteLength = Number

instance showArrayBuffer :: Show ArrayBuffer where
  show = showImpl
  
foreign import showImpl "var showImpl = require('util').inspect;" :: ArrayBuffer -> String

foreign import create
"""
function create(s) {
  return new ArrayBuffer(s);
}
""" :: ByteLength -> ArrayBuffer

foreign import byteLength
"""
function byteLength(a) {
  return a.byteLength;
}
""" :: ArrayBuffer -> ByteLength

foreign import sliceImpl """
  function sliceImpl(s, e, a) {
    return a.slice(s,e);
  }""" :: Fn3 ByteOffset ByteOffset ArrayBuffer ArrayBuffer
slice :: ByteOffset -> ByteOffset -> ArrayBuffer -> ArrayBuffer
slice = runFn3 sliceImpl


foreign import fromArray """
  function fromArray(s) {
    return (new Uint8Array(s)).buffer;
  }
""" :: [Number] -> ArrayBuffer

foreign import fromString """
  function fromString(s) {
    var l = s.length;
    var ab = new ArrayBuffer(l * 2);
    var a = new Uint16Array(ab);
    for (var i = 0; i < l; i++)
      a[i] = s.charCodeAt(i);
    return ab;
  }
""" :: String -> ArrayBuffer
