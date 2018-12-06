module Data.ArrayBuffer.ArrayBuffer ( create
                                    , byteLength
                                    , slice
                                    , fromArray
                                    , fromIntArray
                                   ) where

import Data.ArrayBuffer.Types (ArrayBuffer, ByteOffset, ByteLength)
import Data.Function.Uncurried (Fn3, runFn3)
import Effect (Effect)


-- | Create an `ArrayBuffer` with the given capacity.
foreign import create :: ByteLength -> Effect ArrayBuffer

-- | Represents the length of an `ArrayBuffer` in bytes.
foreign import byteLength :: ArrayBuffer -> ByteLength

foreign import sliceImpl :: Fn3 ByteOffset ByteOffset ArrayBuffer (Effect ArrayBuffer)

-- | Returns a new `ArrayBuffer` whose contents are a copy of this ArrayBuffer's bytes from begin, inclusive, up to end, exclusive.
slice :: ByteOffset -> ByteOffset -> ArrayBuffer -> Effect ArrayBuffer
slice = runFn3 sliceImpl

-- | Convert an array into an `ArrayBuffer` representation.
foreign import fromArray :: Array Number -> ArrayBuffer

-- | Convert an array into an `ArrayBuffer` representation.
foreign import fromIntArray :: Array Int -> ArrayBuffer