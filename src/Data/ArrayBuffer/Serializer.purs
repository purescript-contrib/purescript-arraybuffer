module Data.ArrayBuffer.Serializer where

import Data.ArrayBuffer.Types
import qualified Data.ArrayBuffer as AB
import qualified Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.Advancer
import Data.Function
import Data.Maybe
import Control.Monad.Eff

type Serializer = Advancer

putInt8 s v = advance 1 s >>= DV.setInt8 s.dv v
putInt16 s v = advance 2 s >>= DV.setInt16 s.dv v
putInt32 s v = advance 4 s >>= DV.setInt32 s.dv v
putUint8 s v = advance 1 s >>= DV.setUint8 s.dv v
putUint16 s v = advance 2 s >>= DV.setUint16 s.dv v
putUint32 s v = advance 4 s >>= DV.setUint32 s.dv v
putFloat32 s v = advance 4 s >>= DV.setFloat32 s.dv v
putFloat64 s v = advance 8 s >>= DV.setFloat64 s.dv v

mapDataView :: forall e. Serializer -> ByteLength -> Eff (writer :: DV.Writer | e) (Maybe DV.DataView)
mapDataView s n = do
  o <- advance n s
  return $ DV.slice o n (DV.buffer s.dv)

serializer :: forall e. ByteLength -> Eff (writer :: DV.Writer | e) Serializer
serializer l = return $ { dv : DV.whole $ AB.create l, off : 0 }

close :: Serializer -> forall e. Eff (writer :: DV.Writer | e) AB.ArrayBuffer
close s = return $ AB.slice 0 s.off (DV.buffer s.dv)

serialized :: forall e. ByteLength -> (Serializer -> Eff (writer :: DV.Writer | e) Unit) -> AB.ArrayBuffer
serialized n f = runWPure (do
  s <- serializer n
  f s
  close s)

foreign import runWPure
"""
function runWPure(f) {
  return f();
}
""" :: forall a e. Eff (| e) a -> a
