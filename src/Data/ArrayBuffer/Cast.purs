-- | `DataView` represents unaligned memory of unknown endianness.
-- |
-- | `ArrayView` represents arrays of aligned elements of
-- | local-machine endianness.
-- | For the cases of `Int8Array`, `Uint8Array`, `Uint8ClampedArray`,
-- | the elements
-- | are single bytes, so they are always aligned and they have no
-- | endianness. Therefore in those cases we can freely cast back and forth
-- | to `DataView`.
module Data.ArrayBuffer.Cast
  ( fromInt8Array
  , fromUint8Array
  , fromUint8ClampedArray
  , toInt8Array
  , toUint8Array
  , toUint8ClampedArray
  ) where

import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.Typed as AT
import Data.ArrayBuffer.Types (DataView, Uint8Array, Uint8ClampedArray, Int8Array)
import Effect (Effect)

-- | Cast an `Int8Array` to a `DataView`.
fromInt8Array :: Int8Array -> Effect DataView
fromInt8Array x = DV.part (AT.buffer x) (AT.byteOffset x) (AT.byteLength x)

-- | Cast a `DataView` to an `Int8Array`.
toInt8Array :: DataView -> Effect Int8Array
toInt8Array x = AT.part (DV.buffer x) (DV.byteOffset x) (DV.byteLength x)

-- | Cast a `UInt8Array` to a `DataView`.
fromUint8Array :: Uint8Array -> Effect DataView
fromUint8Array x = DV.part (AT.buffer x) (AT.byteOffset x) (AT.byteLength x)

-- | Cast a `DataView` to a `Uint8Array`.
toUint8Array :: DataView -> Effect Uint8Array
toUint8Array x = AT.part (DV.buffer x) (DV.byteOffset x) (DV.byteLength x)

-- | Cast a `UInt8ClampedArray` to a `DataView`.
fromUint8ClampedArray :: Uint8ClampedArray -> Effect DataView
fromUint8ClampedArray x = DV.part (AT.buffer x) (AT.byteOffset x) (AT.byteLength x)

-- | Cast a `DataView` to a `Uint8ClampedArray`.
toUint8ClampedArray :: DataView -> Effect Uint8ClampedArray
toUint8ClampedArray x = AT.part (DV.buffer x) (DV.byteOffset x) (DV.byteLength x)
