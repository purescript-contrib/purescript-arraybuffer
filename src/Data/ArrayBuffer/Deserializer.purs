module Data.ArrayBuffer.Deserializer where

import Data.Either
import Data.Maybe
import Control.Monad.Eff
import Data.ArrayBuffer.Advancer
import Data.ArrayBuffer.Types
import qualified Data.ArrayBuffer.Typed as TA
import qualified Data.ArrayBuffer.ArrayBuffer as AB
import qualified Data.ArrayBuffer.DataView as DV

type Deserializer = Advancer

type DE a = Eff (reader :: DV.Reader) (Either String a)
type Getter a = Deserializer -> DE a
type ArrayGetter t = Deserializer -> Number -> DE t

chkErr :: forall a. Maybe a -> Either String a
chkErr x = case x of
  (Just v) -> Right v
  otherwise -> Left "Short read"

getInt8 :: Getter Int8
getInt8 d =  chkErr <$> (advance 1 d >>= DV.getInt8 d.dv)
getInt16 :: Getter Int16
getInt16 d = chkErr <$> (advance 2 d >>= DV.getInt16 d.dv)
getInt32 :: Getter Int32
getInt32 d = chkErr <$> (advance 4 d >>= DV.getInt32 d.dv)
getUint8 :: Getter Uint8
getUint8 d = chkErr <$> (advance 1 d >>= DV.getUint8 d.dv)
getUint16 :: Getter Uint16
getUint16 d = chkErr <$> (advance 2 d >>= DV.getUint16 d.dv)
getUint32 :: Getter Uint32
getUint32 d = chkErr <$> (advance 4 d >>= DV.getUint32 d.dv)
getFloat32 :: Getter Float32
getFloat32 d = chkErr <$> (advance 4 d >>= DV.getFloat32 d.dv)
getFloat64 :: Getter Float64
getFloat64 d = chkErr <$> (advance 8 d >>= DV.getFloat64 d.dv)

getDataView :: Deserializer -> ByteLength -> DE DataView
getDataView d n = do
  o <- advance n d
  return $ case DV.slice o n (DV.buffer d.dv) of
    (Just dv) -> Right dv
    otherwise -> Left "short read"

getTypedArray :: forall t. Deserializer -> Number -> (DataView -> t) -> DE t
getTypedArray d sz conv = do
  edv <- getDataView d sz
  return $ case edv of
    Right dv -> Right $ conv dv
    Left err -> Left err

getInt8Array :: ArrayGetter Int8Array
getInt8Array d n = getTypedArray d n TA.asInt8Array
getInt16Array :: ArrayGetter Int16Array
getInt16Array d n = getTypedArray d (n * 2) TA.asInt16Array
getInt32Array :: ArrayGetter Int32Array
getInt32Array d n = getTypedArray d (n * 4) TA.asInt32Array
getUint8Array :: ArrayGetter Uint8Array
getUint8Array d n = getTypedArray d n TA.asUint8Array
getUint16Array :: ArrayGetter Uint16Array
getUint16Array d n = getTypedArray d (n * 2) TA.asUint16Array
getUint32Array :: ArrayGetter Uint32Array
getUint32Array d n = getTypedArray d (n * 4) TA.asUint32Array
getUint8ClampedArray :: ArrayGetter Uint8ClampedArray
getUint8ClampedArray d n = getTypedArray d n TA.asUint8ClampedArray
getFloat32Array :: ArrayGetter Float32Array
getFloat32Array d n = getTypedArray d (n * 4) TA.asFloat32Array
getFloat64Array :: ArrayGetter Float64Array
getFloat64Array d n = getTypedArray d (n * 8) TA.asFloat64Array

deserializer :: ArrayBuffer -> Eff (reader :: DV.Reader) Deserializer
deserializer ab = return $ { dv : DV.whole ab, off : 0 }


deserialized :: forall a. (Deserializer -> Eff (reader :: DV.Reader) (Either String a)) -> ArrayBuffer -> Either String a
deserialized f b = runRPure (do
          d <- deserializer b
          res <- f d
          return res)

foreign import runRPure
"""
function runRPure(f) {
  return f();
}
""" :: forall e a. Eff (|e) a -> a
