-- | This module represents the functional bindings to JavaScript's `DataView`
-- | objects. See [MDN's spec](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DataView) for details.

module Data.ArrayBuffer.DataView
  ( whole
  , remainder
  , part
  , buffer
  , byteOffset
  , byteLength
  , Getter()
  , getInt8
  , getInt16be
  , getInt32be
  , getUint8
  , getUint16be
  , getUint32be
  , getFloat32be
  , getFloat64be
  , getInt16le
  , getInt32le
  , getUint16le
  , getUint32le
  , getFloat32le
  , getFloat64le
  , Setter()
  , setInt8
  , setInt16be
  , setInt32be
  , setUint8
  , setUint16be
  , setUint32be
  , setFloat32be
  , setFloat64be
  , setInt16le
  , setInt32le
  , setUint16le
  , setUint32le
  , setFloat32le
  , setFloat64le
  ) where

import Data.ArrayBuffer.Types
  ( ByteOffset, DataView, ByteLength, ArrayBuffer, kind ArrayViewType
  , Int32, Int16, Int8, Uint32, Uint16, Uint8, Float32, Float64)
import Data.ArrayBuffer.ValueMapping (class BinaryValue)

import Prelude (Unit, const, pure, (<$>))
import Data.Maybe (Maybe(..))
import Data.UInt (UInt)
import Effect (Effect)
import Effect.Exception (catchException)
import Effect.Uncurried (EffectFn5, EffectFn3, EffectFn2, runEffectFn5, runEffectFn3, runEffectFn2)


-- | Type for all fetching functions.
newtype Getter (a :: ArrayViewType) t =
  Getter (DataView -> ByteOffset -> Effect (Maybe t))

-- | Type for all storing functions.
newtype Setter (a :: ArrayViewType) t =
  Setter (DataView -> t -> ByteOffset -> Effect Unit)

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


type Endianness = Boolean

foreign import getterImpl :: forall t. EffectFn5 String ByteLength Endianness DataView ByteOffset t

getter :: forall a t. BinaryValue a t => String -> ByteLength -> Endianness -> Getter a t
getter p l e = Getter \d o ->
  let x = runEffectFn5 getterImpl p l e d o
  in  catchException (const (pure Nothing)) (Just <$> x)

foreign import setterImpl :: forall t. EffectFn5 String Endianness DataView t ByteOffset Unit

setter :: forall a t. BinaryValue a t => String -> Endianness -> Setter a t
setter p e = Setter \d x o ->
  runEffectFn5 setterImpl p e d x o


-- | Fetch int8 value at a certain index in a `DataView`.
getInt8 :: Getter Int8 Int
getInt8 = getter "getInt8" 1 false

-- | Fetch int16 value at a certain index in a `DataView`.
getInt16be :: Getter Int16 Int
getInt16be = getter "getInt16" 2 false

getInt16le :: Getter Int16 Int
getInt16le = getter "getInt16" 2 true

-- | Fetch int32 value at a certain index in a `DataView`.
getInt32be :: Getter Int32 Int
getInt32be = getter "getInt32" 4 false

getInt32le :: Getter Int32 Int
getInt32le = getter "getInt32" 4 true

-- | Fetch uint8 value at a certain index in a `DataView`.
getUint8 :: Getter Uint8 UInt
getUint8 = getter "getUint8" 1 false

-- | Fetch uint16 value at a certain index in a `DataView`.
getUint16be :: Getter Uint16 UInt
getUint16be = getter "getUint16" 2 false

getUint16le :: Getter Uint16 UInt
getUint16le = getter "getUint16" 2 true

-- | Fetch uint32 value at a certain index in a `DataView`.
getUint32be :: Getter Uint32 UInt
getUint32be = getter "getUint32" 4 false

getUint32le :: Getter Uint32 UInt
getUint32le = getter "getUint32" 4 true

-- | Fetch float32 value at a certain index in a `DataView`.
getFloat32be :: Getter Float32 Number
getFloat32be = getter "getFloat32" 4 false

getFloat32le :: Getter Float32 Number
getFloat32le = getter "getFloat32" 4 true

-- | Fetch float64 value at a certain index in a `DataView`.
getFloat64be :: Getter Float64 Number
getFloat64be = getter "getFloat64" 8 false

getFloat64le :: Getter Float64 Number
getFloat64le = getter "getFloat64" 8 true

-- | Store int8 value at a certain index in a `DataView`.
setInt8 :: Setter Int8 Int
setInt8 = setter "setInt8" false

-- | Store int16 value at a certain index in a `DataView`.
setInt16be :: Setter Int16 Int
setInt16be = setter "setInt16" false

setInt16le :: Setter Int16 Int
setInt16le = setter "setInt16" true

-- | Store int32 value at a certain index in a `DataView`.
setInt32be :: Setter Int32 Int
setInt32be = setter "setInt32" false

setInt32le :: Setter Int32 Int
setInt32le = setter "setInt32" true

-- | Store uint8 value at a certain index in a `DataView`.
setUint8 :: Setter Uint8 UInt
setUint8 = setter "setUint8" false

-- | Store uint16 value at a certain index in a `DataView`.
setUint16be :: Setter Uint16 UInt
setUint16be = setter "setUint16" false

setUint16le :: Setter Uint16 UInt
setUint16le = setter "setUint16" true

-- | Store uint32 value at a certain index in a `DataView`.
setUint32be :: Setter Uint32 UInt
setUint32be = setter "setUint32" false

setUint32le :: Setter Uint32 UInt
setUint32le = setter "setUint32" true

-- | Store float32 value at a certain index in a `DataView`.
setFloat32be :: Setter Float32 Number
setFloat32be = setter "setFloat32" false

setFloat32le :: Setter Float32 Number
setFloat32le = setter "setFloat32" true

-- | Store float64 value at a certain index in a `DataView`.
setFloat64be :: Setter Float64 Number
setFloat64be = setter "setFloat64" false

setFloat64le :: Setter Float64 Number
setFloat64le = setter "setFloat64" true
