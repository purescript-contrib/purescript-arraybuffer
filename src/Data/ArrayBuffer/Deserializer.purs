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

getDataView :: forall e. Deserializer -> ByteLength -> Eff (reader :: DV.Reader | e) (Either String DV.DataView)
getDataView d n = do
  o <- advance n d
  return $ case DV.slice o n (DV.buffer d.dv) of
    (Just dv) -> Right dv
    otherwise -> Left "short read"

type AD t = forall e. Deserializer -> Number -> Eff (reader :: DV.Reader | e) (Either String t)


getTypedArray :: forall e t. Deserializer -> Number -> (DV.DataView -> t) -> Eff (reader :: DV.Reader | e) (Either String t)
getTypedArray d sz conv = do
  edv <- getDataView d sz
  return $ case edv of
    Right dv -> Right $ conv dv
    Left err -> Left err

getInt8Array :: AD TA.Int8Array
getInt8Array d n = getTypedArray d n TA.asInt8Array
getInt16Array :: AD TA.Int16Array
getInt16Array d n = getTypedArray d (n * 2) TA.asInt16Array
getInt32Array :: AD TA.Int32Array
getInt32Array d n = getTypedArray d (n * 4) TA.asInt32Array
getUint8Array :: AD TA.Uint8Array
getUint8Array d n = getTypedArray d n TA.asUint8Array
getUint16Array :: AD TA.Uint16Array
getUint16Array d n = getTypedArray d (n * 2) TA.asUint16Array
getUint32Array :: AD TA.Uint32Array
getUint32Array d n = getTypedArray d (n * 4) TA.asUint32Array
getUint8ClampedArray :: AD TA.Uint8ClampedArray
getUint8ClampedArray d n = getTypedArray d n TA.asUint8ClampedArray
getFloat32Array :: AD TA.Float32Array
getFloat32Array d n = getTypedArray d (n * 4) TA.asFloat32Array
getFloat64Array :: AD TA.Float64Array
getFloat64Array d n = getTypedArray d (n * 8) TA.asFloat64Array

deserializer :: forall e. AB.ArrayBuffer -> Eff (reader :: DV.Reader | e) Deserializer
deserializer ab = return $ { dv : DV.whole ab, off : 0 }


deserialized :: forall a e. (Deserializer -> Eff (reader :: DV.Reader | e) (Either String a)) -> AB.ArrayBuffer -> Either String a
deserialized f b = runRPure (do
          d <- deserializer b
          res <- f d
          return res)

type WPure a = forall e. Eff (writer :: DV.Writer |e) a

foreign import runRPure
"""
function runRPure(f) {
  return f();
}
""" :: forall e a. Eff (|e) a -> a
