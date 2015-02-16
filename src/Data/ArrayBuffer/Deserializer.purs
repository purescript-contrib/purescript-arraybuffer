module Data.ArrayBuffer.Deserializer where

import Data.Either
import Data.Maybe
import Control.Monad.Eff
import Data.ArrayBuffer.Advancer
import Data.ArrayBuffer.Types
import qualified Data.ArrayBuffer.Typed as TA
import qualified Data.ArrayBuffer as AB
import qualified Data.ArrayBuffer.DataView as DV

type Deserializer = Advancer

class IsDeserializable a where
  get :: forall e. Deserializer -> Eff (reader :: DV.Reader | e) (Either String a)

chkErr :: forall a e. Maybe a -> Either String a
chkErr x = case x of
  (Just v) -> Right v
  otherwise -> Left "Short read"


getInt8 :: forall e. Deserializer -> Eff (reader :: DV.Reader | e) (Either String Int8)
getInt8 d =  chkErr <$> (advance 1 d >>= DV.getInt8 d.dv)
getInt16 d = chkErr <$> (advance 2 d >>= DV.getInt16 d.dv)
getInt32 d = chkErr <$> (advance 4 d >>= DV.getInt32 d.dv)
getUint8 d = chkErr <$> (advance 1 d >>= DV.getUint8 d.dv)
getUint16 d = chkErr <$> (advance 2 d >>= DV.getUint16 d.dv)
getUint32 d = chkErr <$> (advance 4 d >>= DV.getUint32 d.dv)
getFloat32 d = chkErr <$> (advance 4 d >>= DV.getFloat32 d.dv)
getFloat64 d = chkErr <$> (advance 8 d >>= DV.getFloat64 d.dv)

instance isDeserializableInt8 :: IsDeserializable Int8 where
  get = getInt8
instance isDeserializableInt16 :: IsDeserializable Int16 where
  get = getInt16
instance isDeserializableInt32 :: IsDeserializable Int32 where
  get = getInt32
instance isDeserializableUint8 :: IsDeserializable Uint8 where
  get = getUint8
instance isDeserializableUint16 :: IsDeserializable Uint16 where
  get = getUint16
instance isDeserializableUint32 :: IsDeserializable Uint32 where
  get = getUint32
instance isDeserializableFloat32 :: IsDeserializable Float32 where
  get = getFloat32
instance isDeserializableFloat64 :: IsDeserializable Float64 where
  get = getFloat64

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

