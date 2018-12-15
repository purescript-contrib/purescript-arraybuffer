module Data.ArrayBuffer.ValueMapping where

import Data.ArrayBuffer.Types
  ( kind ArrayViewType
  , Float64, Float32
  , Uint8Clamped, Uint32, Uint16, Uint8, Int32, Int16, Int8)
import Data.Typelevel.Num (D1, D2, D4, D8)
import Data.UInt (UInt)


class BytesPerValue (a :: ArrayViewType) (b :: Type) | a -> b

instance bytesPerValueUint8Clamped :: BytesPerValue Uint8Clamped D1
instance bytesPerValueUint32 :: BytesPerValue Uint32 D4
instance bytesPerValueUint16 :: BytesPerValue Uint16 D2
instance bytesPerValueUint8 :: BytesPerValue Uint8 D1
instance bytesPerValueInt32 :: BytesPerValue Int32 D4
instance bytesPerValueInt16 :: BytesPerValue Int16 D2
instance bytesPerValueInt8 :: BytesPerValue Int8 D1
instance bytesPerValueFloat32 :: BytesPerValue Float32 D4
instance bytesPerValueFloat64 :: BytesPerValue Float64 D8


class BinaryValue (a :: ArrayViewType) (t :: Type) | a -> t

instance binaryValueUint8Clamped :: BinaryValue Uint8Clamped UInt
instance binaryValueUint32 :: BinaryValue Uint32 UInt
instance binaryValueUint16 :: BinaryValue Uint16 UInt
instance binaryValueUint8 :: BinaryValue Uint8 UInt
instance binaryValueInt32 :: BinaryValue Int32 Int
instance binaryValueInt16 :: BinaryValue Int16 Int
instance binaryValueInt8 :: BinaryValue Int8 Int
instance binaryValueFloat32 :: BinaryValue Float32 Number
instance binaryValueFloat64 :: BinaryValue Float64 Number
