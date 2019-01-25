-- | This module represents the functional bindings to JavaScript's `DataView`
-- | objects. See [MDN's spec](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DataView) for details.

module Data.ArrayBuffer.DataView
       ( AProxy (..)
       , Endian (..)
       , buffer
       , byteLength
       , byteOffset
       , class DataView
       , class ViewTypeName
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
       , nm
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

import Prelude

import Data.ArrayBuffer.Types (ArrayBuffer, ByteLength, ByteOffset, DataView, Float32, Float64, Int16, Int32, Int8, Uint16, Uint32, Uint8, Uint8Clamped, kind ArrayViewType)
import Data.ArrayBuffer.ValueMapping (class BinaryValue, class BytesPerValue)
import Data.Maybe (Maybe(..))
import Data.Typelevel.Num (toInt', class Nat)
import Data.UInt (UInt)
import Effect (Effect)
import Effect.Uncurried (EffectFn2, EffectFn3, EffectFn4, runEffectFn2, runEffectFn3, runEffectFn4)
import Type.Proxy (Proxy(..))



-- | View mapping the whole `ArrayBuffer`.
foreign import whole :: ArrayBuffer -> DataView

foreign import remainderImpl :: EffectFn2 ArrayBuffer ByteOffset DataView

-- | View mapping the rest of an `ArrayBuffer` after an index.
remainder :: ArrayBuffer -> ByteOffset -> Effect DataView
remainder a o = runEffectFn2 remainderImpl a o

foreign import partImpl :: EffectFn3 ArrayBuffer ByteOffset ByteLength DataView

-- | View mapping a region of the `ArrayBuffer`.
part :: ArrayBuffer -> ByteOffset -> ByteLength -> Effect DataView
part a o l = runEffectFn3 partImpl a o l

-- | `ArrayBuffer` being mapped by the view.
foreign import buffer :: DataView -> ArrayBuffer

-- | Represents the offset of this view from the start of its `ArrayBuffer`.
foreign import byteOffset :: DataView -> ByteOffset

-- | Represents the length of this view.
foreign import byteLength :: DataView -> ByteLength


data AProxy (a :: ArrayViewType) = AProxy

data Endian = LE | BE

instance eqEndian :: Eq Endian where
  eq LE LE = true
  eq BE BE = true
  eq _ _ = false

class (BinaryValue a t, ViewTypeName a) <= DataView (a :: ArrayViewType) t | a -> t

instance dataViewUint8Clamped :: DataView Uint8Clamped UInt
instance dataViewUint32 :: DataView Uint32 UInt
instance dataViewUint16 :: DataView Uint16 UInt
instance dataViewUint8 :: DataView Uint8 UInt
instance dataViewInt32 :: DataView Int32 Int
instance dataViewInt16 :: DataView Int16 Int
instance dataViewInt8 :: DataView Int8 Int
instance dataViewFloat32 :: DataView Float32 Number
instance dataViewFloat64 :: DataView Float64 Number


class ViewTypeName vty where
  nm :: AProxy vty -> String

instance viewTypeNameUint8Clamped :: ViewTypeName Uint8Clamped where
  nm _ = "Uint8Clamped"
instance viewTypeNameViewUint32 :: ViewTypeName Uint32 where
  nm _ = "Uint32"
instance viewTypeNameViewUint16 :: ViewTypeName Uint16 where
  nm _ = "Uint16"
instance viewTypeNameViewUint8 :: ViewTypeName Uint8 where
  nm _ = "Uint8"
instance viewTypeNameViewInt32 :: ViewTypeName Int32 where
  nm _ = "Int32"
instance viewTypeNameViewInt16 :: ViewTypeName Int16 where
  nm _ = "Int16"
instance viewTypeNameViewInt8 :: ViewTypeName Int8 where
  nm _ = "Int8"
instance viewTypeNameViewFloat32 :: ViewTypeName Float32 where
  nm _ = "Float32"
instance viewTypeNameViewFloat64 :: ViewTypeName Float64 where
  nm _ = "Float64"

foreign import getterImpl :: forall t
                           . EffectFn3 { just :: t -> Maybe t
                                       , nothing :: Maybe t
                                       , functionName :: String
                                       , littleEndian :: Boolean
                                       , bytesPerValue :: ByteLength
                                       } DataView ByteOffset (Maybe t)

getter :: forall t.
          { functionName :: String
          , bytesPerValue :: ByteLength
          , littleEndian :: Boolean
          }
       -> DataView -> ByteOffset -> Effect (Maybe t)
getter data' d o =
  runEffectFn3 getterImpl
    { just: Just
    , nothing: Nothing
    , functionName: data'.functionName
    , littleEndian: data'.littleEndian
    , bytesPerValue: data'.bytesPerValue
    } d o


get :: forall a t b. DataView a t => BytesPerValue a b => Nat b => Endian -> AProxy a -> DataView -> ByteOffset -> Effect (Maybe t)
get endian prx =
  let le = endian == LE
      pnm = "get" <> nm prx
      bpv = toInt' (Proxy :: Proxy b)
  in getter { functionName: pnm
            , bytesPerValue: bpv
            , littleEndian: le
            }

getBE :: forall a t b. DataView a t => BytesPerValue a b => Nat b => AProxy a -> DataView -> ByteOffset -> Effect (Maybe t)
getBE = get BE

getLE :: forall a t b. DataView a t => BytesPerValue a b => Nat b => AProxy a -> DataView -> ByteOffset -> Effect (Maybe t)
getLE = get LE

foreign import setterImpl :: forall t
                           . EffectFn4 { functionName :: String
                                       , littleEndian :: Boolean
                                       , bytesPerValue :: ByteLength
                                       } DataView ByteOffset t Boolean

setter :: forall t.
          { functionName :: String
          , bytesPerValue :: ByteLength
          , littleEndian :: Boolean
          } -> DataView -> ByteOffset -> t -> Effect Boolean
setter d o t = runEffectFn4 setterImpl d o t

set :: forall a t b. DataView a t => BytesPerValue a b => Nat b => Endian -> AProxy a -> DataView -> ByteOffset -> t -> Effect Boolean
set endian prx =
  let le = endian == LE
      pnm = "set" <> nm prx
      bpv = toInt' (Proxy :: Proxy b)
  in setter { functionName: pnm
            , bytesPerValue: bpv
            , littleEndian: le
            }

-- | Fetch int8 value at a certain index in a `DataView`.
getInt8 :: DataView -> ByteOffset -> Effect (Maybe Int)
getInt8 = getLE (AProxy :: AProxy Int8)

-- | Fetch big-endian int16 value at a certain index in a `DataView`.
getInt16be :: DataView -> ByteOffset -> Effect (Maybe Int)
getInt16be = getBE (AProxy :: AProxy Int16)

-- | Fetch little-endian int16 value at a certain index in a `DataView`.
getInt16le :: DataView -> ByteOffset -> Effect (Maybe Int)
getInt16le = getLE (AProxy :: AProxy Int16)

-- | Fetch big-endian int32 value at a certain index in a `DataView`.
getInt32be :: DataView -> ByteOffset -> Effect (Maybe Int)
getInt32be = getBE (AProxy :: AProxy Int32)

-- | Fetch little-endian int32 value at a certain index in a `DataView`.
getInt32le :: DataView -> ByteOffset -> Effect (Maybe Int)
getInt32le = getLE (AProxy :: AProxy Int32)

-- | Fetch uint8 value at a certain index in a `DataView`.
getUint8 :: DataView -> ByteOffset -> Effect (Maybe UInt)
getUint8 = getLE (AProxy :: AProxy Uint8)

-- | Fetch big-endian uint16 value at a certain index in a `DataView`.
getUint16be :: DataView -> ByteOffset -> Effect (Maybe UInt)
getUint16be = getBE (AProxy :: AProxy Uint16)

-- | Fetch little-endian uint16 value at a certain index in a `DataView`.
getUint16le :: DataView -> ByteOffset -> Effect (Maybe UInt)
getUint16le = getLE (AProxy :: AProxy Uint16)

-- | Fetch big-endian uint32 value at a certain index in a `DataView`.
getUint32be :: DataView -> ByteOffset -> Effect (Maybe UInt)
getUint32be = getBE (AProxy :: AProxy Uint32)

-- | Fetch little-endian uint32 value at a certain index in a `DataView`.
getUint32le :: DataView -> ByteOffset -> Effect (Maybe UInt)
getUint32le = getLE (AProxy :: AProxy Uint32)

-- | Fetch big-endian float32 value at a certain index in a `DataView`.
getFloat32be :: DataView -> ByteOffset -> Effect (Maybe Number)
getFloat32be = getBE (AProxy :: AProxy Float32)

-- | Fetch little-endian float32 value at a certain index in a `DataView`.
getFloat32le :: DataView -> ByteOffset -> Effect (Maybe Number)
getFloat32le = getLE (AProxy :: AProxy Float32)

-- | Fetch big-endian float64 value at a certain index in a `DataView`.
getFloat64be :: DataView -> ByteOffset -> Effect (Maybe Number)
getFloat64be = getBE (AProxy :: AProxy Float64)

-- | Fetch little-endian float64 value at a certain index in a `DataView`.
getFloat64le :: DataView -> ByteOffset -> Effect (Maybe Number)
getFloat64le = getLE (AProxy :: AProxy Float64)


-- | Store big-endian value at a certain index in a `DataView`.
setBE :: forall a t b. DataView a t => BytesPerValue a b => Nat b => AProxy a -> DataView -> ByteOffset -> t -> Effect Boolean
setBE = set BE

-- | Store little-endian value at a certain index in a `DataView`.
setLE :: forall a t b. DataView a t => BytesPerValue a b => Nat b => AProxy a -> DataView -> ByteOffset -> t -> Effect Boolean
setLE = set LE

-- | Store int8 value at a certain index in a `DataView`.
setInt8 :: DataView -> ByteOffset -> Int -> Effect Boolean
setInt8 = setLE (AProxy :: AProxy Int8)

-- | Store big-endian int16 value at a certain index in a `DataView`.
setInt16be :: DataView -> ByteOffset -> Int -> Effect Boolean
setInt16be = setBE (AProxy :: AProxy Int16)

-- | Store little-endian int16 value at a certain index in a `DataView`.
setInt16le :: DataView -> ByteOffset -> Int -> Effect Boolean
setInt16le = setLE (AProxy :: AProxy Int16)

-- | Store big-endian int32 value at a certain index in a `DataView`.
setInt32be :: DataView -> ByteOffset -> Int -> Effect Boolean
setInt32be = setBE (AProxy :: AProxy Int32)

-- | Store little-endian int32 value at a certain index in a `DataView`.
setInt32le :: DataView -> ByteOffset -> Int -> Effect Boolean
setInt32le = setLE (AProxy :: AProxy Int32)

-- | Store uint8 value at a certain index in a `DataView`.
setUint8 :: DataView -> ByteOffset -> UInt -> Effect Boolean
setUint8 = setLE (AProxy :: AProxy Uint8)


-- | Store big-endian uint16 value at a certain index in a `DataView`.
setUint16be :: DataView -> ByteOffset -> UInt -> Effect Boolean
setUint16be = setBE (AProxy :: AProxy Uint16)

-- | Store little-endian uint16 value at a certain index in a `DataView`.
setUint16le :: DataView -> ByteOffset -> UInt -> Effect Boolean
setUint16le = setLE (AProxy :: AProxy Uint16)

-- | Store big-endian uint32 value at a certain index in a `DataView`.
setUint32be :: DataView -> ByteOffset -> UInt -> Effect Boolean
setUint32be = setBE (AProxy :: AProxy Uint32)

-- | Store little-endian uint32 value at a certain index in a `DataView`.
setUint32le :: DataView -> ByteOffset -> UInt -> Effect Boolean
setUint32le = setLE (AProxy :: AProxy Uint32)

-- | Store big-endian float32 value at a certain index in a `DataView`.
setFloat32be :: DataView -> ByteOffset -> Number -> Effect Boolean
setFloat32be = setBE (AProxy :: AProxy Float32)

-- | Store little-endian float32 value at a certain index in a `DataView`.
setFloat32le :: DataView -> ByteOffset -> Number -> Effect Boolean
setFloat32le = setLE (AProxy :: AProxy Float32)

-- | Store big-endian float64 value at a certain index in a `DataView`.
setFloat64be :: DataView -> ByteOffset -> Number -> Effect Boolean
setFloat64be = setBE (AProxy :: AProxy Float64)

-- | Store little-endian float64 value at a certain index in a `DataView`.
setFloat64le :: DataView -> ByteOffset -> Number -> Effect Boolean
setFloat64le = setLE (AProxy :: AProxy Float64)
