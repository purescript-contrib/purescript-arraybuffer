module Data.ArrayBuffer.Serializer where

import Data.ArrayBuffer.Types
import qualified Data.ArrayBuffer as AB
import qualified Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.Advancer
import Data.Function
import Data.Maybe
import Control.Monad.Eff

type Serializer = Advancer

type SE = Eff (writer :: DV.Writer) Serializer

putter :: forall a. Number -> DV.Setter a -> a -> Serializer -> Eff (writer :: DV.Writer) Serializer
putter n f v s = do
  o <- advance n s
  f s.dv v o
  return s

putInt8 :: Int8 -> Serializer -> SE
putInt8 = putter 1 DV.setInt8
putInt16 :: Int16 -> Serializer -> SE
putInt16 = putter 2 DV.setInt16
putInt32 :: Int32 -> Serializer -> SE
putInt32 = putter 4 DV.setInt32
putUint8 :: Uint8 -> Serializer -> SE
putUint8 = putter 1 DV.setUint8
putUint16 :: Uint16 -> Serializer -> SE
putUint16 = putter 2 DV.setUint16
putUint32 :: Uint32 -> Serializer -> SE
putUint32 = putter 4 DV.setUint32
putFloat32 :: Float32 -> Serializer -> SE
putFloat32 = putter 4 DV.setFloat32
putFloat64 :: Float64 -> Serializer -> SE
putFloat64 = putter 8 DV.setFloat64

mapDataView :: forall e. ByteLength -> Serializer -> Eff (writer :: DV.Writer | e) (Maybe DV.DataView)
mapDataView n s = do
  o <- advance n s
  return $ DV.slice o n (DV.buffer s.dv)

serializer :: forall e. ByteLength -> Eff (writer :: DV.Writer | e) Serializer
serializer l = return $ { dv : DV.whole $ AB.create l, off : 0 }

close :: Serializer -> forall e. Eff (writer :: DV.Writer | e) AB.ArrayBuffer
close s = return $ AB.slice 0 s.off (DV.buffer s.dv)

serialized :: forall e. ByteLength -> (Serializer -> Eff (writer :: DV.Writer | e) Serializer) -> AB.ArrayBuffer
serialized n f = runWPure (serializer n >>= f >>= close)

foreign import runWPure
"""
function runWPure(f) {
  return f();
}
""" :: forall a e. Eff (| e) a -> a
