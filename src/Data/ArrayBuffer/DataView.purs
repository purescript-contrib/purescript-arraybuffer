-- | This module represents the functional bindings to JavaScript's `DataView`
-- | objects. See [MDN's spec](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DataView) for details.

module Data.ArrayBuffer.DataView
  ( whole
  , remainder
  , part
  , buffer
  , byteOffset
  , byteLength
  , DVProxy (..), kind Endianness, BE, LE
  , class DataView
  , get, set
  ) where

import Data.ArrayBuffer.Types
  ( ByteOffset, DataView, ByteLength, ArrayBuffer, kind ArrayViewType
  , Int32, Int16, Int8, Uint32, Uint16, Uint8, Float32, Float64)
import Data.ArrayBuffer.ValueMapping (class BinaryValue, class BytesPerValue)

import Prelude (Unit)
import Data.Maybe (Maybe(..))
import Data.UInt (UInt)
import Data.Typelevel.Num (toInt', class Nat)
import Type.Proxy (Proxy (..))
import Effect (Effect)
import Effect.Uncurried
  ( EffectFn4, EffectFn3, EffectFn2
  , runEffectFn4, runEffectFn3, runEffectFn2)




-- | View mapping the whole `ArrayBuffer`.
foreign import whole :: ArrayBuffer -> DataView

foreign import remainderImpl :: EffectFn2 ArrayBuffer ByteOffset DataView

-- | View mapping the rest of an `ArrayBuffer` after an index.
remainder :: ArrayBuffer -> ByteOffset -> Effect DataView
remainder = runEffectFn2 remainderImpl

foreign import partImpl :: EffectFn3 ArrayBuffer ByteOffset ByteLength DataView

-- | View mapping a region of the `ArrayBuffer`.
part :: ArrayBuffer -> ByteOffset -> ByteLength -> Effect DataView
part = runEffectFn3 partImpl

-- | `ArrayBuffer` being mapped by the view.
foreign import buffer :: DataView -> ArrayBuffer

-- | Represents the offset of this view from the start of its `ArrayBuffer`.
foreign import byteOffset :: DataView -> ByteOffset

-- | Represents the length of this view.
foreign import byteLength :: DataView -> ByteLength



data DVProxy (a :: ArrayViewType) (e :: Endianness) = DVProxy

foreign import kind Endianness
foreign import data BE :: Endianness
foreign import data LE :: Endianness


class BinaryValue a t <= DataView (a :: ArrayViewType) (e :: Endianness) t | a -> t where
  get :: DVProxy a e -> DataView -> ByteOffset -> Effect (Maybe t)
  set :: DVProxy a e -> DataView -> t -> ByteOffset -> Effect Unit


foreign import getterImpl :: forall t
                           . EffectFn3 { just :: t -> Maybe t
                                       , nothing :: Maybe t
                                       , functionName :: String
                                       , endian :: Boolean
                                       , bytesPerValue :: ByteLength
                                       } DataView ByteOffset (Maybe t)

getter :: forall t
        . { functionName :: String
          , bytesPerValue :: ByteLength
          , endian :: Boolean
          }
       -> DataView -> ByteOffset -> Effect (Maybe t)
getter data' =
  runEffectFn3 getterImpl
    { just: Just
    , nothing: Nothing
    , functionName: data'.functionName
    , endian: data'.endian
    , bytesPerValue: data'.bytesPerValue
    }

foreign import setterImpl :: forall t. EffectFn4 {functionName :: String, endian :: Boolean} DataView t ByteOffset Unit

setter :: forall t. {functionName :: String, endian :: Boolean} -> DataView -> t -> ByteOffset -> Effect Unit
setter = runEffectFn4 setterImpl


instance dataViewInt8BE :: (BytesPerValue Int8 b, Nat b) => DataView Int8 BE Int where
  get DVProxy = getter {functionName: "getInt8", bytesPerValue: toInt' (Proxy :: Proxy b), endian: false}
  set DVProxy = setter {functionName: "setInt8", endian: false}

instance dataViewInt8LE :: (BytesPerValue Int8 b, Nat b) => DataView Int8 LE Int where
  get DVProxy = getter {functionName: "getInt8", bytesPerValue: toInt' (Proxy :: Proxy b), endian: true}
  set DVProxy = setter {functionName: "setInt8", endian: true}

instance dataViewInt16BE :: (BytesPerValue Int16 b, Nat b) => DataView Int16 BE Int where
  get DVProxy = getter {functionName: "getInt16", bytesPerValue: toInt' (Proxy :: Proxy b), endian: false}
  set DVProxy = setter {functionName: "setInt16", endian: false}

instance dataViewInt16LE :: (BytesPerValue Int16 b, Nat b) => DataView Int16 LE Int where
  get DVProxy = getter {functionName: "getInt16", bytesPerValue: toInt' (Proxy :: Proxy b), endian: true}
  set DVProxy = setter {functionName: "setInt16", endian: true}

instance dataViewInt32BE :: (BytesPerValue Int32 b, Nat b) => DataView Int32 BE Int where
  get DVProxy = getter {functionName: "getInt32", bytesPerValue: toInt' (Proxy :: Proxy b), endian: false}
  set DVProxy = setter {functionName: "setInt32", endian: false}

instance dataViewInt32LE :: (BytesPerValue Int32 b, Nat b) => DataView Int32 LE Int where
  get DVProxy = getter {functionName: "getInt32", bytesPerValue: toInt' (Proxy :: Proxy b), endian: true}
  set DVProxy = setter {functionName: "setInt32", endian: true}

instance dataViewUint8BE :: (BytesPerValue Uint8 b, Nat b) => DataView Uint8 BE UInt where
  get DVProxy = getter {functionName: "getUint8", bytesPerValue: toInt' (Proxy :: Proxy b), endian: false}
  set DVProxy = setter {functionName: "setUint8", endian: false}

instance dataViewUint8LE :: (BytesPerValue Uint8 b, Nat b) => DataView Uint8 LE UInt where
  get DVProxy = getter {functionName: "getUint8", bytesPerValue: toInt' (Proxy :: Proxy b), endian: true}
  set DVProxy = setter {functionName: "setUint8", endian: true}

instance dataViewUint16BE :: (BytesPerValue Uint16 b, Nat b) => DataView Uint16 BE UInt where
  get DVProxy = getter {functionName: "getUint16", bytesPerValue: toInt' (Proxy :: Proxy b), endian: false}
  set DVProxy = setter {functionName: "setUint16", endian: false}

instance dataViewUint16LE :: (BytesPerValue Uint16 b, Nat b) => DataView Uint16 LE UInt where
  get DVProxy = getter {functionName: "getUint16", bytesPerValue: toInt' (Proxy :: Proxy b), endian: true}
  set DVProxy = setter {functionName: "setUint16", endian: true}

instance dataViewUint32BE :: (BytesPerValue Uint32 b, Nat b) => DataView Uint32 BE UInt where
  get DVProxy = getter {functionName: "getUint32", bytesPerValue: toInt' (Proxy :: Proxy b), endian: false}
  set DVProxy = setter {functionName: "setUint32", endian: false}

instance dataViewUint32LE :: (BytesPerValue Uint32 b, Nat b) => DataView Uint32 LE UInt where
  get DVProxy = getter {functionName: "getUint32", bytesPerValue: toInt' (Proxy :: Proxy b), endian: true}
  set DVProxy = setter {functionName: "setUint32", endian: true}

instance dataViewFloat32BE :: (BytesPerValue Float32 b, Nat b) => DataView Float32 BE Number where
  get DVProxy = getter {functionName: "getFloat32", bytesPerValue: toInt' (Proxy :: Proxy b), endian: false}
  set DVProxy = setter {functionName: "setFloat32", endian: false}

instance dataViewFloat32LE :: (BytesPerValue Float32 b, Nat b) => DataView Float32 LE Number where
  get DVProxy = getter {functionName: "getFloat32", bytesPerValue: toInt' (Proxy :: Proxy b), endian: true}
  set DVProxy = setter {functionName: "setFloat32", endian: true}

instance dataViewFloat64BE :: (BytesPerValue Float64 b, Nat b) => DataView Float64 BE Number where
  get DVProxy = getter {functionName: "getFloat64", bytesPerValue: toInt' (Proxy :: Proxy b), endian: false}
  set DVProxy = setter {functionName: "setFloat64", endian: false}

instance dataViewFloat64LE :: (BytesPerValue Float64 b, Nat b) => DataView Float64 LE Number where
  get DVProxy = getter {functionName: "getFloat64", bytesPerValue: toInt' (Proxy :: Proxy b), endian: true}
  set DVProxy = setter {functionName: "setFloat64", endian: true}
