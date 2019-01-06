-- | This module represents the functional bindings to JavaScript's `DataView`
-- | objects. See [MDN's spec](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DataView) for details.

module Data.ArrayBuffer.DataView
  ( whole
  , remainder
  , part
  , buffer
  , byteOffset
  , byteLength
  , AProxy (..)
  , class DataView
  , getBE, getLE, setBE, setLE
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


data AProxy (a :: ArrayViewType) = AProxy


class BinaryValue a t <= DataView (a :: ArrayViewType) t | a -> t where
  getLE :: AProxy a -> DataView -> ByteOffset -> Effect (Maybe t)
  getBE :: AProxy a -> DataView -> ByteOffset -> Effect (Maybe t)
  setBE :: AProxy a -> DataView -> t -> ByteOffset -> Effect Boolean
  setLE :: AProxy a -> DataView -> t -> ByteOffset -> Effect Boolean


foreign import getterImpl :: forall t
                           . EffectFn3 { just :: t -> Maybe t
                                       , nothing :: Maybe t
                                       , functionName :: String
                                       , littleEndian :: Boolean
                                       , bytesPerValue :: ByteLength
                                       } DataView ByteOffset (Maybe t)

getter :: forall t
        . { functionName :: String
          , bytesPerValue :: ByteLength
          , littleEndian :: Boolean
          }
       -> DataView -> ByteOffset -> Effect (Maybe t)
getter data' =
  runEffectFn3 getterImpl
    { just: Just
    , nothing: Nothing
    , functionName: data'.functionName
    , littleEndian: data'.littleEndian
    , bytesPerValue: data'.bytesPerValue
    }

foreign import setterImpl :: forall t
                           . EffectFn4 { functionName :: String
                                       , littleEndian :: Boolean
                                       , bytesPerValue :: ByteLength
                                       } DataView t ByteOffset Boolean

setter :: forall t
        . { functionName :: String
          , bytesPerValue :: ByteLength
          , littleEndian :: Boolean
          } -> DataView -> t -> ByteOffset -> Effect Boolean
setter = runEffectFn4 setterImpl


instance dataViewInt8 :: (BytesPerValue Int8 b, Nat b) => DataView Int8 Int where
  getBE AProxy = getter {functionName: "getInt8", bytesPerValue: toInt' (Proxy :: Proxy b), littleEndian: false}
  setBE AProxy = setter {functionName: "setInt8", bytesPerValue: toInt' (Proxy :: Proxy b), littleEndian: false}
  getLE AProxy = getter {functionName: "getInt8", bytesPerValue: toInt' (Proxy :: Proxy b), littleEndian: true}
  setLE AProxy = setter {functionName: "setInt8", bytesPerValue: toInt' (Proxy :: Proxy b), littleEndian: true}

instance dataViewInt16 :: (BytesPerValue Int16 b, Nat b) => DataView Int16 Int where
  getBE AProxy = getter {functionName: "getInt16", bytesPerValue: toInt' (Proxy :: Proxy b), littleEndian: false}
  setBE AProxy = setter {functionName: "setInt16", bytesPerValue: toInt' (Proxy :: Proxy b), littleEndian: false}
  getLE AProxy = getter {functionName: "getInt16", bytesPerValue: toInt' (Proxy :: Proxy b), littleEndian: true}
  setLE AProxy = setter {functionName: "setInt16", bytesPerValue: toInt' (Proxy :: Proxy b), littleEndian: true}

instance dataViewInt32 :: (BytesPerValue Int32 b, Nat b) => DataView Int32 Int where
  getBE AProxy = getter {functionName: "getInt32", bytesPerValue: toInt' (Proxy :: Proxy b), littleEndian: false}
  setBE AProxy = setter {functionName: "setInt32", bytesPerValue: toInt' (Proxy :: Proxy b), littleEndian: false}
  getLE AProxy = getter {functionName: "getInt32", bytesPerValue: toInt' (Proxy :: Proxy b), littleEndian: true}
  setLE AProxy = setter {functionName: "setInt32", bytesPerValue: toInt' (Proxy :: Proxy b), littleEndian: true}

instance dataViewUint8 :: (BytesPerValue Uint8 b, Nat b) => DataView Uint8 UInt where
  getBE AProxy = getter {functionName: "getUint8", bytesPerValue: toInt' (Proxy :: Proxy b), littleEndian: false}
  setBE AProxy = setter {functionName: "setUint8", bytesPerValue: toInt' (Proxy :: Proxy b), littleEndian: false}
  getLE AProxy = getter {functionName: "getUint8", bytesPerValue: toInt' (Proxy :: Proxy b), littleEndian: true}
  setLE AProxy = setter {functionName: "setUint8", bytesPerValue: toInt' (Proxy :: Proxy b), littleEndian: true}

instance dataViewUint16 :: (BytesPerValue Uint16 b, Nat b) => DataView Uint16 UInt where
  getBE AProxy = getter {functionName: "getUint16", bytesPerValue: toInt' (Proxy :: Proxy b), littleEndian: false}
  setBE AProxy = setter {functionName: "setUint16", bytesPerValue: toInt' (Proxy :: Proxy b), littleEndian: false}
  getLE AProxy = getter {functionName: "getUint16", bytesPerValue: toInt' (Proxy :: Proxy b), littleEndian: true}
  setLE AProxy = setter {functionName: "setUint16", bytesPerValue: toInt' (Proxy :: Proxy b), littleEndian: true}

instance dataViewUint32 :: (BytesPerValue Uint32 b, Nat b) => DataView Uint32 UInt where
  getBE AProxy = getter {functionName: "getUint32", bytesPerValue: toInt' (Proxy :: Proxy b), littleEndian: false}
  setBE AProxy = setter {functionName: "setUint32", bytesPerValue: toInt' (Proxy :: Proxy b), littleEndian: false}
  getLE AProxy = getter {functionName: "getUint32", bytesPerValue: toInt' (Proxy :: Proxy b), littleEndian: true}
  setLE AProxy = setter {functionName: "setUint32", bytesPerValue: toInt' (Proxy :: Proxy b), littleEndian: true}

instance dataViewFloat32 :: (BytesPerValue Float32 b, Nat b) => DataView Float32 Number where
  getBE AProxy = getter {functionName: "getFloat32", bytesPerValue: toInt' (Proxy :: Proxy b), littleEndian: false}
  setBE AProxy = setter {functionName: "setFloat32", bytesPerValue: toInt' (Proxy :: Proxy b), littleEndian: false}
  getLE AProxy = getter {functionName: "getFloat32", bytesPerValue: toInt' (Proxy :: Proxy b), littleEndian: true}
  setLE AProxy = setter {functionName: "setFloat32", bytesPerValue: toInt' (Proxy :: Proxy b), littleEndian: true}

instance dataViewFloat64 :: (BytesPerValue Float64 b, Nat b) => DataView Float64 Number where
  getBE AProxy = getter {functionName: "getFloat64", bytesPerValue: toInt' (Proxy :: Proxy b), littleEndian: false}
  setBE AProxy = setter {functionName: "setFloat64", bytesPerValue: toInt' (Proxy :: Proxy b), littleEndian: false}
  getLE AProxy = getter {functionName: "getFloat64", bytesPerValue: toInt' (Proxy :: Proxy b), littleEndian: true}
  setLE AProxy = setter {functionName: "setFloat64", bytesPerValue: toInt' (Proxy :: Proxy b), littleEndian: true}
