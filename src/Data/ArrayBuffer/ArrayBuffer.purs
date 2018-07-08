module Data.ArrayBuffer.ArrayBuffer ( create
                                    , byteLength
                                    , slice
                                    , fromArray
                                    , fromIntArray
                                    , fromString
                                    , decodeToString
                                   ) where

import Data.ArrayBuffer.DataView (whole, buffer)
import Data.ArrayBuffer.Typed (asUint8Array, dataView)
import Data.ArrayBuffer.Types (ArrayBuffer, ByteOffset, ByteLength)
import Data.Either (Either)
import Data.Function.Uncurried (Fn3, runFn3)
import Data.TextDecoder (decodeUtf8)
import Data.TextEncoder (encodeUtf8)
import Effect (Effect)
import Effect.Exception (Error)
import Prelude ((<<<))


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

-- | Convert a UTF-8 encoded `ArrayBuffer` into a `String`.
-- | Serves as a quick utility function for a common use-case. For more use-cases,
-- | see: [purescript-text-encoding](https://pursuit.purescript.org/packages/purescript-text-encoding/0.0.8)
-- | Requires the TextDecoder class available. A polyfill can be found in the npm package "text-encoding"
decodeToString :: ArrayBuffer -> Either Error String
decodeToString = decodeUtf8 <<< asUint8Array <<< whole

-- | Convert a  `String` into a UTF-8 encoded `ArrayBuffer`.
-- | Serves as a quick utility function for a common use-case. For more use-cases,
-- | see: [purescript-text-encoding](https://pursuit.purescript.org/packages/purescript-text-encoding/0.0.8)
-- | Requires the TextDecoder class available. A polyfill can be found in the npm package "text-encoding"
fromString :: String -> ArrayBuffer
fromString = buffer <<< dataView <<< encodeUtf8
