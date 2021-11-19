-- | This module represents the functional bindings to JavaScript's `ArrayBuffer`
-- | objects. See [MDN's spec](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/ArrayBuffer) for details.
module Data.ArrayBuffer.ArrayBuffer
  ( empty
  , byteLength
  , slice
  ) where

import Data.ArrayBuffer.Types (ArrayBuffer, ByteOffset, ByteLength)
import Data.Function.Uncurried (Fn3, runFn3)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)

-- | Create an `ArrayBuffer` with the given capacity.
empty :: ByteLength -> Effect ArrayBuffer
empty l = runEffectFn1 emptyImpl l

foreign import emptyImpl :: EffectFn1 ByteLength ArrayBuffer

-- | Represents the length of an `ArrayBuffer` in bytes.
foreign import byteLength :: ArrayBuffer -> ByteLength

-- | Returns a new `ArrayBuffer` whose contents are a copy of this ArrayBuffer's bytes from begin, inclusive, up to end, exclusive.
slice :: ByteOffset -> ByteOffset -> ArrayBuffer -> ArrayBuffer
slice s e a = runFn3 sliceImpl a s e

foreign import sliceImpl :: Fn3 ArrayBuffer ByteOffset ByteOffset ArrayBuffer
