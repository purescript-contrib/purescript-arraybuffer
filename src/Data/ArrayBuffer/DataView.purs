module Data.ArrayBuffer.DataView( Reader()
                                , Writer()
                                , whole
                                , slice
                                , buffer
                                , byteOffset
                                , byteLength
                                , Getter()
                                , getInt8
                                , getInt16
                                , getInt32
                                , getUint8
                                , getUint16
                                , getUint32
                                , getFloat32
                                , getFloat64
                                , Setter()
                                , setInt8
                                , setInt16
                                , setInt32
                                , setUint8
                                , setUint16
                                , setUint32
                                , setFloat32
                                , setFloat64
                                ) where

import Prelude
import Data.ArrayBuffer.Types
import Data.Function
import Data.Maybe
import Control.Monad.Eff

-- | Type for all fetching functions.
type Getter r = forall e. DataView -> ByteOffset -> Eff (reader :: Reader | e) (Maybe r)

-- | Type for all storing functions.
type Setter r = forall e. DataView -> r -> ByteOffset -> Eff (writer :: Writer | e) Unit

-- | View mapping the whole `ArrayBuffer`.
foreign import whole :: ArrayBuffer -> DataView

foreign import sliceImpl :: forall e. Fn5 (DataView -> Maybe DataView) (Maybe DataView) ByteOffset ByteLength ArrayBuffer (Maybe DataView)

-- | View mapping a region of the `ArrayBuffer`.
slice :: forall e. ByteOffset -> ByteLength -> ArrayBuffer -> Maybe DataView
slice = runFn5 sliceImpl Just Nothing

-- | `ArrayBuffer` being mapped by the view.
foreign import buffer :: DataView -> ArrayBuffer

-- | Represents the offset of this view from the start of its `ArrayBuffer`.
foreign import byteOffset :: DataView -> ByteOffset

-- | Represents the length of this view.
foreign import byteLength :: DataView -> ByteLength


foreign import data Reader :: !

foreign import getterImpl :: forall e r. Fn6 (r -> Maybe r) (Maybe r) String ByteLength DataView ByteOffset (Eff (reader :: Reader | e) (Maybe r))

getter :: forall e r. String -> ByteLength -> DataView -> ByteOffset -> Eff (reader :: Reader | e) (Maybe r)
getter = runFn6 getterImpl Just Nothing


foreign import data Writer :: !

foreign import setter :: forall e r. String -> DataView -> r -> ByteOffset -> Eff (writer :: Writer | e) Unit


-- | Fetch int8 value at a certain index in a `DataView`.
getInt8 :: Getter Number
getInt8 = getter "getInt8" 1

-- | Fetch int16 value at a certain index in a `DataView`.
getInt16 :: Getter Number
getInt16 = getter "getInt16" 2

-- | Fetch int32 value at a certain index in a `DataView`.
getInt32 :: Getter Number
getInt32 = getter "getInt32" 4

-- | Fetch uint8 value at a certain index in a `DataView`.
getUint8 :: Getter Number
getUint8 = getter "getUint8" 1

-- | Fetch uint16 value at a certain index in a `DataView`.
getUint16 :: Getter Number
getUint16 = getter "getUint16" 2

-- | Fetch uint32 value at a certain index in a `DataView`.
getUint32 :: Getter Number
getUint32 = getter "getUint32" 4

-- | Fetch float32 value at a certain index in a `DataView`.
getFloat32 :: Getter Number
getFloat32 = getter "getFloat32" 4

-- | Fetch float64 value at a certain index in a `DataView`.
getFloat64 :: Getter Number
getFloat64 = getter "getFloat64" 8

-- | Store int8 value at a certain index in a `DataView`.
setInt8 :: Setter Number
setInt8 = setter "setInt8"

-- | Store int16 value at a certain index in a `DataView`.
setInt16 :: Setter Number
setInt16 = setter "setInt16"

-- | Store int32 value at a certain index in a `DataView`.
setInt32 :: Setter Number
setInt32 = setter "setInt32"

-- | Store uint8 value at a certain index in a `DataView`.
setUint8 :: Setter Number
setUint8 = setter "setUint8"

-- | Store uint16 value at a certain index in a `DataView`.
setUint16 :: Setter Number
setUint16 = setter "setUint16"

-- | Store uint32 value at a certain index in a `DataView`.
setUint32 :: Setter Number
setUint32 = setter "setUint32"

-- | Store float32 value at a certain index in a `DataView`.
setFloat32 :: Setter Number
setFloat32 = setter "setFloat32"

-- | Store float64 value at a certain index in a `DataView`.
setFloat64 :: Setter Number
setFloat64 = setter "setFloat64"
