module Data.ArrayBuffer.Deserializer where

import Data.ArrayBuffer.Types
import qualified Data.ArrayBuffer as AB
import qualified Data.ArrayBuffer.DataView as DV
import Data.Function
import Data.ArrayBuffer.Advancer
import Control.Monad.Eff

type Deserializer = Advancer

class IsDeserializable a where
  get :: forall e. Deserializer -> Eff (reader :: DV.Reader | e) a

instance isDeserializableInt8 :: IsDeserializable Int8 where
  get d = advance 1 d >>= DV.getInt8 d.dv
instance isDeserializableInt16 :: IsDeserializable Int16 where
  get d = advance 2 d >>= DV.getInt16 d.dv
instance isDeserializableInt32 :: IsDeserializable Int32 where
  get d = advance 4 d >>= DV.getInt32 d.dv
instance isDeserializableUint8 :: IsDeserializable Uint8 where
  get d = advance 1 d >>= DV.getUint8 d.dv
instance isDeserializableUint16 :: IsDeserializable Uint16 where
  get d = advance 2 d >>= DV.getUint16 d.dv
instance isDeserializableUint32 :: IsDeserializable Uint32 where
  get d = advance 4 d >>= DV.getUint32 d.dv
instance isDeserializableFloat32 :: IsDeserializable Float32 where
  get d = advance 4 d >>= DV.getFloat32 d.dv
instance isDeserializableFloat64 :: IsDeserializable Float64 where
  get d = advance 8 d >>= DV.getFloat64 d.dv

deserializer :: forall e. AB.ArrayBuffer -> Eff (reader :: DV.Reader | e) Deserializer
deserializer ab = return $ { dv : DV.whole ab, off : 0 }

