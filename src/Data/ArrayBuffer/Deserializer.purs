module Data.ArrayBuffer.Deserializer where

import Data.Function
import Control.Monad.Eff
import Data.ArrayBuffer.Advancer
import Data.ArrayBuffer.Types
import qualified Data.ArrayBuffer.Typed as TA
import qualified Data.ArrayBuffer as AB
import qualified Data.ArrayBuffer.DataView as DV

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

getDataView :: forall e. Deserializer -> ByteLength -> Eff (reader :: DV.Reader | e) DV.DataView
getDataView d n = do
  o <- advance n d
  return $ DV.slice o n (DV.buffer d.dv)

type AD t = forall e. Deserializer -> Number -> Eff (reader :: DV.Reader | e) t

getInt8Array :: AD TA.Int8Array
getInt8Array d n = getDataView d n >>= return <<< TA.asInt8Array
getInt16Array :: AD TA.Int16Array
getInt16Array d n = getDataView d (n * 2) >>= return <<< TA.asInt16Array
getInt32Array :: AD TA.Int32Array
getInt32Array d n = getDataView d (n * 4) >>= return <<< TA.asInt32Array
getUint8Array :: AD TA.Uint8Array
getUint8Array d n = getDataView d n >>= return <<< TA.asUint8Array
getUint16Array :: AD TA.Uint16Array
getUint16Array d n = getDataView d (n * 2) >>= return <<< TA.asUint16Array
getUint32Array :: AD TA.Uint32Array
getUint32Array d n = getDataView d (n * 4) >>= return <<< TA.asUint32Array
getUint8ClampedArray :: AD TA.Uint8ClampedArray
getUint8ClampedArray d n = getDataView d n >>= return <<< TA.asUint8ClampedArray
getFloat32Array :: AD TA.Float32Array
getFloat32Array d n = getDataView d (n * 4) >>= return <<< TA.asFloat32Array
getFloat64Array :: AD TA.Float64Array
getFloat64Array d n = getDataView d (n * 8) >>= return <<< TA.asFloat64Array

deserializer :: forall e. AB.ArrayBuffer -> Eff (reader :: DV.Reader | e) Deserializer
deserializer ab = return $ { dv : DV.whole ab, off : 0 }

