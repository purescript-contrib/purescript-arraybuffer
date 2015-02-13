module Data.ArrayBuffer.Serializer where

import Data.ArrayBuffer.Types
import qualified Data.ArrayBuffer as AB
import qualified Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.Advancer
import Data.Function
import Control.Monad.Eff

type Serializer = Advancer

class IsSerializable a where
  put :: forall e. Serializer -> a -> Eff (writer :: DV.Writer | e) Unit

instance isSerializableInt8 :: IsSerializable Int8 where
  put s v = advance 1 s >>= DV.setInt8 s.dv v
instance isSerializableInt16 :: IsSerializable Int16 where
  put s v = advance 2 s >>= DV.setInt16 s.dv v
instance isSerializableInt32 :: IsSerializable Int32 where
  put s v = advance 4 s >>= DV.setInt32 s.dv v
instance isSerializableUint8 :: IsSerializable Uint8 where
  put s v = advance 1 s >>= DV.setUint8 s.dv v
instance isSerializableUint16 :: IsSerializable Uint16 where
  put s v = advance 2 s >>= DV.setUint16 s.dv v
instance isSerializableUint32 :: IsSerializable Uint32 where
  put s v = advance 4 s >>= DV.setUint32 s.dv v
instance isSerializableFloat32 :: IsSerializable Float32 where
  put s v = advance 4 s >>= DV.setFloat32 s.dv v
instance isSerializableFloat64 :: IsSerializable Float64 where
  put s v = advance 8 s >>= DV.setFloat64 s.dv v



serializer :: forall e. ByteLength -> Eff (writer :: DV.Writer | e) Serializer
serializer l = return $ { dv : DV.whole $ AB.create l, off : 0 }

close :: Serializer -> forall e. Eff (writer :: DV.Writer | e) AB.ArrayBuffer
close s = return $ AB.slice 0 s.off (DV.buffer s.dv)

serialized :: forall e. ByteLength -> (Serializer -> Eff (writer :: DV.Writer | e) Unit) -> Eff (writer :: DV.Writer | e) AB.ArrayBuffer
serialized n f = do
  s <- serializer n
  f s
  close s
