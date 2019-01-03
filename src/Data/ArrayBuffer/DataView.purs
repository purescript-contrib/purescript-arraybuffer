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
  ( EffectFn5, EffectFn6, EffectFn3, EffectFn2
  , runEffectFn5, runEffectFn6, runEffectFn3, runEffectFn2)




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
                           . EffectFn6 { just :: t -> Maybe t
                                       , nothing :: Maybe t
                                       } String ByteLength Boolean DataView ByteOffset (Maybe t)

getter :: forall t. String -> ByteLength -> Boolean -> DataView -> ByteOffset -> Effect (Maybe t)
getter p l e = \d o -> runEffectFn6 getterImpl {just: Just, nothing: Nothing} p l e d o

foreign import setterImpl :: forall t. EffectFn5 String Boolean DataView t ByteOffset Unit

setter :: forall t. String -> Boolean -> DataView -> t -> ByteOffset -> Effect Unit
setter p e = \d x o ->
  runEffectFn5 setterImpl p e d x o


instance dataViewInt8BE :: (BytesPerValue Int8 b, Nat b) => DataView Int8 BE Int where
  get DVProxy = getter "getInt8" (toInt' (Proxy :: Proxy b)) false
  set DVProxy = setter "setInt8" false

instance dataViewInt8LE :: (BytesPerValue Int8 b, Nat b) => DataView Int8 LE Int where
  get DVProxy = getter "getInt8" (toInt' (Proxy :: Proxy b)) true
  set DVProxy = setter "setInt8" true

instance dataViewInt16BE :: (BytesPerValue Int16 b, Nat b) => DataView Int16 BE Int where
  get DVProxy = getter "getInt16" (toInt' (Proxy :: Proxy b)) false
  set DVProxy = setter "setInt16" false

instance dataViewInt16LE :: (BytesPerValue Int16 b, Nat b) => DataView Int16 LE Int where
  get DVProxy = getter "getInt16" (toInt' (Proxy :: Proxy b)) true
  set DVProxy = setter "setInt16" true

instance dataViewInt32BE :: (BytesPerValue Int32 b, Nat b) => DataView Int32 BE Int where
  get DVProxy = getter "getInt32" (toInt' (Proxy :: Proxy b)) false
  set DVProxy = setter "setInt32" false

instance dataViewInt32LE :: (BytesPerValue Int32 b, Nat b) => DataView Int32 LE Int where
  get DVProxy = getter "getInt32" (toInt' (Proxy :: Proxy b)) true
  set DVProxy = setter "setInt32" true

instance dataViewUint8BE :: (BytesPerValue Uint8 b, Nat b) => DataView Uint8 BE UInt where
  get DVProxy = getter "getUint8" (toInt' (Proxy :: Proxy b)) false
  set DVProxy = setter "setUint8" false

instance dataViewUint8LE :: (BytesPerValue Uint8 b, Nat b) => DataView Uint8 LE UInt where
  get DVProxy = getter "getUint8" (toInt' (Proxy :: Proxy b)) true
  set DVProxy = setter "setUint8" true

instance dataViewUint16BE :: (BytesPerValue Uint16 b, Nat b) => DataView Uint16 BE UInt where
  get DVProxy = getter "getUint16" (toInt' (Proxy :: Proxy b)) false
  set DVProxy = setter "setUint16" false

instance dataViewUint16LE :: (BytesPerValue Uint16 b, Nat b) => DataView Uint16 LE UInt where
  get DVProxy = getter "getUint16" (toInt' (Proxy :: Proxy b)) true
  set DVProxy = setter "setUint16" true

instance dataViewUint32BE :: (BytesPerValue Uint32 b, Nat b) => DataView Uint32 BE UInt where
  get DVProxy = getter "getUint32" (toInt' (Proxy :: Proxy b)) false
  set DVProxy = setter "setUint32" false

instance dataViewUint32LE :: (BytesPerValue Uint32 b, Nat b) => DataView Uint32 LE UInt where
  get DVProxy = getter "getUint32" (toInt' (Proxy :: Proxy b)) true
  set DVProxy = setter "setUint32" true

instance dataViewFloat32BE :: (BytesPerValue Float32 b, Nat b) => DataView Float32 BE Number where
  get DVProxy = getter "getFloat32" (toInt' (Proxy :: Proxy b)) false
  set DVProxy = setter "setFloat32" false

instance dataViewFloat32LE :: (BytesPerValue Float32 b, Nat b) => DataView Float32 LE Number where
  get DVProxy = getter "getFloat32" (toInt' (Proxy :: Proxy b)) true
  set DVProxy = setter "setFloat32" true

instance dataViewFloat64BE :: (BytesPerValue Float64 b, Nat b) => DataView Float64 BE Number where
  get DVProxy = getter "getFloat64" (toInt' (Proxy :: Proxy b)) false
  set DVProxy = setter "setFloat64" false

instance dataViewFloat64LE :: (BytesPerValue Float64 b, Nat b) => DataView Float64 LE Number where
  get DVProxy = getter "getFloat64" (toInt' (Proxy :: Proxy b)) true
  set DVProxy = setter "setFloat64" true
