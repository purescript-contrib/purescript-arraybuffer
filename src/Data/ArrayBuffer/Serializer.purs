module Data.ArrayBuffer.Serializer where

import Data.ArrayBuffer.Types
import qualified Data.ArrayBuffer as AB
import qualified Data.ArrayBuffer.DataView as DV
import Data.Function
import Control.Monad.Eff

foreign import advance """
function advance(d) {
  return function(s) {
    return function() {
      d.off += s;
    };
  };
}
""" :: forall e. Serializer -> AB.ByteOffset -> Eff (writer :: DV.Writer | e) Unit

newtype Serializer = Serializer { dv :: DV.DataView, off :: AB.ByteOffset }

class IsSerializable a where
  put :: forall e. Serializer -> a -> Eff (writer :: DV.Writer | e) Unit

instance isSerializableInt8 :: IsSerializable Int8 where
  put ss@(Serializer s) v = do
    DV.setInt8 s.dv s.off v
    advance ss 1
    
serializer :: forall e. AB.ByteLength -> Eff (writer :: DV.Writer | e) Serializer
serializer l = return $ Serializer { dv : DV.whole $ AB.create l, off : 0 }

close :: Serializer -> forall e. Eff (writer :: DV.Writer | e) AB.ArrayBuffer
close (Serializer s) = return $ AB.slice 0 s.off (DV.buffer s.dv)

serialized :: forall e. (Serializer -> Eff (writer :: DV.Writer | e) Unit) -> Eff (writer :: DV.Writer | e) AB.ArrayBuffer
serialized f = do
  s <- serializer 1000
  f s
  close s
