module Data.ArrayBuffer.Deserializer where

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
""" :: forall e. Deserializer -> AB.ByteOffset -> Eff (reader :: DV.Reader | e) Unit
newtype Deserializer = Deserializer { dv :: DV.DataView, off :: AB.ByteOffset }

class IsDeserializable a where
  get :: forall e. Deserializer -> Eff (reader :: DV.Reader | e) a

instance isDeserializableInt8 :: IsDeserializable Int8 where
  get ds@(Deserializer d) = do
    v <- DV.getInt8 d.dv d.off
    advance ds 1
    return v

deserializer :: forall e. AB.ArrayBuffer -> Eff (reader :: DV.Reader | e) Deserializer
deserializer ab = return $ Deserializer { dv : DV.whole ab, off : 0 }

