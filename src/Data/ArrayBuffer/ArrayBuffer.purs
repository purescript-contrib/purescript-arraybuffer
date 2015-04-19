module Data.ArrayBuffer.ArrayBuffer where

import Data.Function
import Data.ArrayBuffer.Types


-- | Create an `ArrayBuffer` with the given capacity.
foreign import create
"""
function create(s) {
  return new ArrayBuffer(s);
}
""" :: ByteLength -> ArrayBuffer

-- | Represents the length of an `ArrayBuffer` in bytes.
foreign import byteLength
"""
function byteLength(a) {
  return a.byteLength;
}
""" :: ArrayBuffer -> ByteLength

foreign import sliceImpl
"""
function sliceImpl(s, e, a) {
  return a.slice(s,e);
}
""" :: Fn3 ByteOffset ByteOffset ArrayBuffer ArrayBuffer

-- | Returns a new `ArrayBuffer` whose contents are a copy of this ArrayBuffer's bytes from begin, inclusive, up to end, exclusive.
slice :: ByteOffset -> ByteOffset -> ArrayBuffer -> ArrayBuffer
slice = runFn3 sliceImpl

-- | Convert an array into an `ArrayBuffer` representation.
foreign import fromArray
"""
function fromArray(s) {
  return (new Uint8Array(s)).buffer;
}
""" :: [Number] -> ArrayBuffer

-- | Convert a string into an `ArrayBuffer` representation.
foreign import fromString
"""
function fromString(s) {
  var l = s.length;
  var ab = new ArrayBuffer(l * 2);
  var a = new Uint16Array(ab);
  for (var i = 0; i < l; i++)
    a[i] = s.charCodeAt(i);
  return ab;
}
""" :: String -> ArrayBuffer
