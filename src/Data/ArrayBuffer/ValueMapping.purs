-- | This module represents type-level mappings between `ArrayViewType`s
-- | and meaningful data.
module Data.ArrayBuffer.ValueMapping
  ( class BytesPerType
  , byteWidth
  , class BinaryValue
  , class ShowArrayViewType
  ) where

import Data.ArrayBuffer.Types (ArrayViewType, Float32, Float64, Int16, Int32, Int8, Uint16, Uint32, Uint8, Uint8Clamped)
import Data.Float32 (Float32) as F
import Data.UInt (UInt)
import Type.Proxy (Proxy)

-- | Type-level map of each `ArrayViewType` to the number of bytes of storage
-- | it requires.
class BytesPerType (a :: ArrayViewType) where
  byteWidth :: (Proxy a) -> Int

instance bytesPerTypeInt8 :: BytesPerType Int8 where
  byteWidth _ = 1

instance bytesPerTypeInt16 :: BytesPerType Int16 where
  byteWidth _ = 2

instance bytesPerTypeInt32 :: BytesPerType Int32 where
  byteWidth _ = 4

instance bytesPerTypeUint8 :: BytesPerType Uint8 where
  byteWidth _ = 1

instance bytesPerTypeUint16 :: BytesPerType Uint16 where
  byteWidth _ = 2

instance bytesPerTypeUint32 :: BytesPerType Uint32 where
  byteWidth _ = 4

instance bytesPerTypeUint8Clamped :: BytesPerType Uint8Clamped where
  byteWidth _ = 1

instance bytesPerTypeFloat32 :: BytesPerType Float32 where
  byteWidth _ = 4

instance bytesPerTypeFloat64 :: BytesPerType Float64 where
  byteWidth _ = 8

-- | Type-level map of `TypedArray`’s binary casted value to its
-- | representation in JavaScript.
class BinaryValue (a :: ArrayViewType) (t :: Type) | a -> t

instance binaryValueUint8Clamped :: BinaryValue Uint8Clamped UInt
instance binaryValueUint32 :: BinaryValue Uint32 UInt
instance binaryValueUint16 :: BinaryValue Uint16 UInt
instance binaryValueUint8 :: BinaryValue Uint8 UInt
instance binaryValueInt32 :: BinaryValue Int32 Int
instance binaryValueInt16 :: BinaryValue Int16 Int
instance binaryValueInt8 :: BinaryValue Int8 Int
instance binaryValueFloat32 :: BinaryValue Float32 F.Float32
instance binaryValueFloat64 :: BinaryValue Float64 Number

-- | Type-level map of `TypedArray` to its element type name.
class ShowArrayViewType (a :: ArrayViewType) (name :: Symbol) | a -> name

instance showArrayViewTypeUint8Clamped :: ShowArrayViewType Uint8Clamped "Uint8Clamped"
instance showArrayViewTypeViewUint32 :: ShowArrayViewType Uint32 "Uint32"
instance showArrayViewTypeViewUint16 :: ShowArrayViewType Uint16 "Uint16"
instance showArrayViewTypeViewUint8 :: ShowArrayViewType Uint8 "Uint8"
instance showArrayViewTypeViewInt32 :: ShowArrayViewType Int32 "Int32"
instance showArrayViewTypeViewInt16 :: ShowArrayViewType Int16 "Int16"
instance showArrayViewTypeViewInt8 :: ShowArrayViewType Int8 "Int8"
instance showArrayViewTypeViewFloat32 :: ShowArrayViewType Float32 "Float32"
instance showArrayViewTypeViewFloat64 :: ShowArrayViewType Float64 "Float64"
