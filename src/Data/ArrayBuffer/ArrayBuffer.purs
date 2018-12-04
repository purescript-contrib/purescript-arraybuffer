-- | This module represents the functional bindings to JavaScript's `ArrayBuffer`
-- | objects. See [MDN's spec](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/ArrayBuffer) for details.

module Data.ArrayBuffer.ArrayBuffer ( empty
                                    , byteLength
                                    , slice
                                    ) where

import Data.ArrayBuffer.Types (ArrayBuffer, ByteOffset, ByteLength)
import Data.Function.Uncurried (Fn3, runFn3)


-- | Create an `ArrayBuffer` with the given capacity.
foreign import empty :: ByteLength -> ArrayBuffer

-- | Represents the length of an `ArrayBuffer` in bytes.
foreign import byteLength :: ArrayBuffer -> ByteLength

foreign import sliceImpl :: Fn3 ByteOffset ByteOffset ArrayBuffer ArrayBuffer

-- | Returns a new `ArrayBuffer` whose contents are a copy of this ArrayBuffer's bytes from begin, inclusive, up to end, exclusive.
slice :: ByteOffset -> ByteOffset -> ArrayBuffer -> ArrayBuffer
slice = runFn3 sliceImpl
