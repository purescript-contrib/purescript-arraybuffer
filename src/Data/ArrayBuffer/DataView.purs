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

import Prelude
import Data.ArrayBuffer.Types (ByteOffset, DataView, ByteLength, ArrayBuffer)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (catchException)
import Effect.Uncurried (EffectFn5, EffectFn3, EffectFn2, runEffectFn5, runEffectFn3, runEffectFn2)
import Data.UInt (UInt)

-- | Type for all fetching functions.
type Getter r = DataView -> ByteOffset -> Effect (Maybe r)

-- | Type for all storing functions.
type Setter r = DataView -> r -> ByteOffset -> Effect Unit

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

foreign import getterImpl :: forall r. EffectFn5 String ByteLength Endianness DataView ByteOffset r

getter :: forall r. String -> ByteLength -> Endianness -> Getter r
getter p l e d o =
  let x = runEffectFn5 getterImpl p l e d o
  in  catchException (const (pure Nothing)) (Just <$> x)

foreign import setterImpl :: forall r. EffectFn5 String Endianness DataView r ByteOffset Unit

setter :: forall r. String -> Endianness -> Setter r
setter p e d x o =
  runEffectFn5 setterImpl p e d x o


-- | Fetch int8 value at a certain index in a `DataView`.
getInt8 :: Getter Int
getInt8 = getter "getInt8" 1 false

-- | Fetch int16 value at a certain index in a `DataView`.
getInt16be :: Getter Int
getInt16be = getter "getInt16" 2 false

getInt16le :: Getter Int
getInt16le = getter "getInt16" 2 true

-- | Fetch int32 value at a certain index in a `DataView`.
getInt32be :: Getter Int
getInt32be = getter "getInt32" 4 false

getInt32le :: Getter Int
getInt32le = getter "getInt32" 4 true

-- | Fetch uint8 value at a certain index in a `DataView`.
getUint8 :: Getter UInt
getUint8 = getter "getUint8" 1 false

-- | Fetch uint16 value at a certain index in a `DataView`.
getUint16be :: Getter UInt
getUint16be = getter "getUint16" 2 false

getUint16le :: Getter UInt
getUint16le = getter "getUint16" 2 true

-- | Fetch uint32 value at a certain index in a `DataView`.
getUint32be :: Getter UInt
getUint32be = getter "getUint32" 4 false

getUint32le :: Getter UInt
getUint32le = getter "getUint32" 4 true

-- | Fetch float32 value at a certain index in a `DataView`.
getFloat32be :: Getter Number
getFloat32be = getter "getFloat32" 4 false

getFloat32le :: Getter Number
getFloat32le = getter "getFloat32" 4 true

-- | Fetch float64 value at a certain index in a `DataView`.
getFloat64be :: Getter Number
getFloat64be = getter "getFloat64" 8 false

getFloat64le :: Getter Number
getFloat64le = getter "getFloat64" 8 true

-- | Store int8 value at a certain index in a `DataView`.
setInt8 :: Setter Int
setInt8 = setter "setInt8" false

-- | Store int16 value at a certain index in a `DataView`.
setInt16be :: Setter Int
setInt16be = setter "setInt16" false

setInt16le :: Setter Int
setInt16le = setter "setInt16" true

-- | Store int32 value at a certain index in a `DataView`.
setInt32be :: Setter Int
setInt32be = setter "setInt32" false

setInt32le :: Setter Int
setInt32le = setter "setInt32" true

-- | Store uint8 value at a certain index in a `DataView`.
setUint8 :: Setter UInt
setUint8 = setter "setUint8" false

-- | Store uint16 value at a certain index in a `DataView`.
setUint16be :: Setter UInt
setUint16be = setter "setUint16" false

setUint16le :: Setter UInt
setUint16le = setter "setUint16" true

-- | Store uint32 value at a certain index in a `DataView`.
setUint32be :: Setter UInt
setUint32be = setter "setUint32" false

setUint32le :: Setter UInt
setUint32le = setter "setUint32" true

-- | Store float32 value at a certain index in a `DataView`.
setFloat32be :: Setter Number
setFloat32be = setter "setFloat32" false

setFloat32le :: Setter Number
setFloat32le = setter "setFloat32" true

-- | Store float64 value at a certain index in a `DataView`.
setFloat64be :: Setter Number
setFloat64be = setter "setFloat64" false

setFloat64le :: Setter Number
setFloat64le = setter "setFloat64" true
