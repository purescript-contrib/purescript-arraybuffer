-- | This module represents the functional bindings to JavaScript's `DataView`
-- | objects. See [MDN's spec](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DataView) for details.
module Data.ArrayBuffer.DataView
  ( Endian(..)
  , buffer
  , byteLength
  , byteOffset
  , get
  , getBE
  , getFloat32be
  , getFloat32le
  , getFloat64be
  , getFloat64le
  , getInt16be
  , getInt16le
  , getInt32be
  , getInt32le
  , getInt8
  , getLE
  , getUint16be
  , getUint16le
  , getUint32be
  , getUint32le
  , getUint8
  , part
  , remainder
  , set
  , setBE
  , setFloat32be
  , setFloat32le
  , setFloat64be
  , setFloat64le
  , setInt16be
  , setInt16le
  , setInt32be
  , setInt32le
  , setInt8
  , setLE
  , setUint16be
  , setUint16le
  , setUint32be
  , setUint32le
  , setUint8
  , whole
  ) where

import Data.ArrayBuffer.Types (ArrayBuffer, ByteLength, ByteOffset, DataView, Float32, Float64, Int16, Int32, Int8, Uint16, Uint32, Uint8)
import Data.ArrayBuffer.ValueMapping (class BinaryValue, class BytesPerType, class ShowArrayViewType, byteWidth)
import Data.Float32 (Float32) as F
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.UInt (UInt)
import Effect (Effect)
import Effect.Uncurried (EffectFn2, EffectFn3, EffectFn4, runEffectFn2, runEffectFn3, runEffectFn4)
import Prelude (class Eq, (<$>), (<>), (==))
import Type.Proxy (Proxy(..))

-- | Endianness of a multi-byte type. Little-Endian or Big-Endian.
data Endian = LE | BE

instance eqEndian :: Eq Endian where
  eq LE LE = true
  eq BE BE = true
  eq _ _ = false

-- | View mapping the whole `ArrayBuffer`.
foreign import whole :: ArrayBuffer -> DataView

-- | View mapping the rest of an `ArrayBuffer` after an index.
remainder :: ArrayBuffer -> ByteOffset -> Effect DataView
remainder a o = runEffectFn2 remainderImpl a o

foreign import remainderImpl :: EffectFn2 ArrayBuffer ByteOffset DataView

-- | View mapping a region of the `ArrayBuffer`.
part :: ArrayBuffer -> ByteOffset -> ByteLength -> Effect DataView
part a o l = runEffectFn3 partImpl a o l

foreign import partImpl :: EffectFn3 ArrayBuffer ByteOffset ByteLength DataView

-- | `ArrayBuffer` being mapped by the view.
foreign import buffer :: DataView -> ArrayBuffer

-- | Represents the offset of this view from the start of its `ArrayBuffer`.
foreign import byteOffset :: DataView -> ByteOffset

-- | Represents the length of this view.
foreign import byteLength :: DataView -> ByteLength

getter
  :: forall t
   . { functionName :: String
     , bytesPerValue :: ByteLength
     , littleEndian :: Boolean
     }
  -> DataView
  -> ByteOffset
  -> Effect (Maybe t)
getter data' dataView offset =
  toMaybe <$> runEffectFn3 getterImpl
    { functionName: data'.functionName
    , littleEndian: data'.littleEndian
    , bytesPerValue: data'.bytesPerValue
    }
    dataView
    offset

foreign import getterImpl
  :: forall t
   . EffectFn3
       { functionName :: String
       , littleEndian :: Boolean
       , bytesPerValue :: ByteLength
       }
       DataView
       ByteOffset
       (Nullable t)

get
  :: forall a name t
   . BinaryValue a t
  => BytesPerType a
  => ShowArrayViewType a name
  => IsSymbol name
  => Endian
  -> Proxy a
  -> DataView
  -> ByteOffset
  -> Effect (Maybe t)
get endian prx = do
  let
    le = endian == LE
    pnm = "get" <> reflectSymbol (Proxy :: Proxy name)
    bpv = byteWidth prx

  getter
    { functionName: pnm
    , bytesPerValue: bpv
    , littleEndian: le
    }

getBE
  :: forall a name t
   . BinaryValue a t
  => BytesPerType a
  => ShowArrayViewType a name
  => IsSymbol name
  => Proxy a
  -> DataView
  -> ByteOffset
  -> Effect (Maybe t)
getBE = get BE

getLE
  :: forall a name t
   . BinaryValue a t
  => BytesPerType a
  => ShowArrayViewType a name
  => IsSymbol name
  => Proxy a
  -> DataView
  -> ByteOffset
  -> Effect (Maybe t)
getLE = get LE

setter
  :: forall t
   . { functionName :: String
     , bytesPerValue :: ByteLength
     , littleEndian :: Boolean
     }
  -> DataView
  -> ByteOffset
  -> t
  -> Effect Boolean
setter dataView offset t = runEffectFn4 setterImpl dataView offset t

foreign import setterImpl
  :: forall t
   . EffectFn4
       { functionName :: String
       , littleEndian :: Boolean
       , bytesPerValue :: ByteLength
       }
       DataView
       ByteOffset
       t
       Boolean

set
  :: forall a name t
   . BinaryValue a t
  => BytesPerType a
  => ShowArrayViewType a name
  => IsSymbol name
  => Endian
  -> Proxy a
  -> DataView
  -> ByteOffset
  -> t
  -> Effect Boolean
set endian prx = do
  let
    le = endian == LE
    pnm = "set" <> reflectSymbol (Proxy :: Proxy name)
    bpv = byteWidth prx

  setter
    { functionName: pnm
    , bytesPerValue: bpv
    , littleEndian: le
    }

-- | Fetch int8 value at a certain index in a `DataView`.
getInt8 :: DataView -> ByteOffset -> Effect (Maybe Int)
getInt8 = getLE (Proxy :: Proxy Int8)

-- | Fetch big-endian int16 value at a certain index in a `DataView`.
getInt16be :: DataView -> ByteOffset -> Effect (Maybe Int)
getInt16be = getBE (Proxy :: Proxy Int16)

-- | Fetch little-endian int16 value at a certain index in a `DataView`.
getInt16le :: DataView -> ByteOffset -> Effect (Maybe Int)
getInt16le = getLE (Proxy :: Proxy Int16)

-- | Fetch big-endian int32 value at a certain index in a `DataView`.
getInt32be :: DataView -> ByteOffset -> Effect (Maybe Int)
getInt32be = getBE (Proxy :: Proxy Int32)

-- | Fetch little-endian int32 value at a certain index in a `DataView`.
getInt32le :: DataView -> ByteOffset -> Effect (Maybe Int)
getInt32le = getLE (Proxy :: Proxy Int32)

-- | Fetch uint8 value at a certain index in a `DataView`.
getUint8 :: DataView -> ByteOffset -> Effect (Maybe UInt)
getUint8 = getLE (Proxy :: Proxy Uint8)

-- | Fetch big-endian uint16 value at a certain index in a `DataView`.
getUint16be :: DataView -> ByteOffset -> Effect (Maybe UInt)
getUint16be = getBE (Proxy :: Proxy Uint16)

-- | Fetch little-endian uint16 value at a certain index in a `DataView`.
getUint16le :: DataView -> ByteOffset -> Effect (Maybe UInt)
getUint16le = getLE (Proxy :: Proxy Uint16)

-- | Fetch big-endian uint32 value at a certain index in a `DataView`.
getUint32be :: DataView -> ByteOffset -> Effect (Maybe UInt)
getUint32be = getBE (Proxy :: Proxy Uint32)

-- | Fetch little-endian uint32 value at a certain index in a `DataView`.
getUint32le :: DataView -> ByteOffset -> Effect (Maybe UInt)
getUint32le = getLE (Proxy :: Proxy Uint32)

-- | Fetch big-endian float32 value at a certain index in a `DataView`.
getFloat32be :: DataView -> ByteOffset -> Effect (Maybe F.Float32)
getFloat32be = getBE (Proxy :: Proxy Float32)

-- | Fetch little-endian float32 value at a certain index in a `DataView`.
getFloat32le :: DataView -> ByteOffset -> Effect (Maybe F.Float32)
getFloat32le = getLE (Proxy :: Proxy Float32)

-- | Fetch big-endian float64 value at a certain index in a `DataView`.
getFloat64be :: DataView -> ByteOffset -> Effect (Maybe Number)
getFloat64be = getBE (Proxy :: Proxy Float64)

-- | Fetch little-endian float64 value at a certain index in a `DataView`.
getFloat64le :: DataView -> ByteOffset -> Effect (Maybe Number)
getFloat64le = getLE (Proxy :: Proxy Float64)

-- | Store big-endian value at a certain index in a `DataView`.
setBE
  :: forall a name t
   . BinaryValue a t
  => BytesPerType a
  => ShowArrayViewType a name
  => IsSymbol name
  => Proxy a
  -> DataView
  -> ByteOffset
  -> t
  -> Effect Boolean
setBE = set BE

-- | Store little-endian value at a certain index in a `DataView`.
setLE
  :: forall a name t
   . BinaryValue a t
  => BytesPerType a
  => ShowArrayViewType a name
  => IsSymbol name
  => Proxy a
  -> DataView
  -> ByteOffset
  -> t
  -> Effect Boolean
setLE = set LE

-- | Store int8 value at a certain index in a `DataView`.
setInt8 :: DataView -> ByteOffset -> Int -> Effect Boolean
setInt8 = setLE (Proxy :: Proxy Int8)

-- | Store big-endian int16 value at a certain index in a `DataView`.
setInt16be :: DataView -> ByteOffset -> Int -> Effect Boolean
setInt16be = setBE (Proxy :: Proxy Int16)

-- | Store little-endian int16 value at a certain index in a `DataView`.
setInt16le :: DataView -> ByteOffset -> Int -> Effect Boolean
setInt16le = setLE (Proxy :: Proxy Int16)

-- | Store big-endian int32 value at a certain index in a `DataView`.
setInt32be :: DataView -> ByteOffset -> Int -> Effect Boolean
setInt32be = setBE (Proxy :: Proxy Int32)

-- | Store little-endian int32 value at a certain index in a `DataView`.
setInt32le :: DataView -> ByteOffset -> Int -> Effect Boolean
setInt32le = setLE (Proxy :: Proxy Int32)

-- | Store uint8 value at a certain index in a `DataView`.
setUint8 :: DataView -> ByteOffset -> UInt -> Effect Boolean
setUint8 = setLE (Proxy :: Proxy Uint8)

-- | Store big-endian uint16 value at a certain index in a `DataView`.
setUint16be :: DataView -> ByteOffset -> UInt -> Effect Boolean
setUint16be = setBE (Proxy :: Proxy Uint16)

-- | Store little-endian uint16 value at a certain index in a `DataView`.
setUint16le :: DataView -> ByteOffset -> UInt -> Effect Boolean
setUint16le = setLE (Proxy :: Proxy Uint16)

-- | Store big-endian uint32 value at a certain index in a `DataView`.
setUint32be :: DataView -> ByteOffset -> UInt -> Effect Boolean
setUint32be = setBE (Proxy :: Proxy Uint32)

-- | Store little-endian uint32 value at a certain index in a `DataView`.
setUint32le :: DataView -> ByteOffset -> UInt -> Effect Boolean
setUint32le = setLE (Proxy :: Proxy Uint32)

-- | Store big-endian float32 value at a certain index in a `DataView`.
setFloat32be :: DataView -> ByteOffset -> F.Float32 -> Effect Boolean
setFloat32be = setBE (Proxy :: Proxy Float32)

-- | Store little-endian float32 value at a certain index in a `DataView`.
setFloat32le :: DataView -> ByteOffset -> F.Float32 -> Effect Boolean
setFloat32le = setLE (Proxy :: Proxy Float32)

-- | Store big-endian float64 value at a certain index in a `DataView`.
setFloat64be :: DataView -> ByteOffset -> Number -> Effect Boolean
setFloat64be = setBE (Proxy :: Proxy Float64)

-- | Store little-endian float64 value at a certain index in a `DataView`.
setFloat64le :: DataView -> ByteOffset -> Number -> Effect Boolean
setFloat64le = setLE (Proxy :: Proxy Float64)
