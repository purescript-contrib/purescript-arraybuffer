-- | This module represents the functional bindings to JavaScript's `ArrayBuffer`
-- | objects. See [MDN's spec](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/ArrayBuffer) for details.

module Data.ArrayBuffer.ArrayBuffer
  ( empty
  , byteLength
  , slice
  ) where

import Data.ArrayBuffer.Types (ArrayBuffer, ByteOffset, ByteLength)
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toNullable)
import Data.Tuple (Tuple(..))


-- | Create an `ArrayBuffer` with the given capacity.
foreign import empty :: ByteLength -> ArrayBuffer

-- | Represents the length of an `ArrayBuffer` in bytes.
foreign import byteLength :: ArrayBuffer -> ByteLength

foreign import sliceImpl :: Fn3 ArrayBuffer (Nullable ByteOffset) (Nullable ByteOffset) ArrayBuffer

-- | Returns a new `ArrayBuffer` whose contents are a copy of this ArrayBuffer's bytes from begin, inclusive, up to end, exclusive.
slice :: ArrayBuffer -> Maybe (Tuple ByteOffset (Maybe ByteOffset)) -> ArrayBuffer
slice a mz = case mz of
  Nothing -> runFn3 sliceImpl a (toNullable Nothing) (toNullable Nothing)
  Just (Tuple s me) -> case me of
    Nothing -> runFn3 sliceImpl a (toNullable (Just s)) (toNullable Nothing)
    Just e -> runFn3 sliceImpl a (toNullable (Just s)) (toNullable (Just e))
