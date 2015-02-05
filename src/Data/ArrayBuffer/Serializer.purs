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
    
serializer :: forall e. ByteLength -> Eff (writer :: DV.Writer | e) Serializer
serializer l = return $ { dv : DV.whole $ AB.create l, off : 0 }

close :: Serializer -> forall e. Eff (writer :: DV.Writer | e) AB.ArrayBuffer
close s = return $ AB.slice 0 s.off (DV.buffer s.dv)

serialized :: forall e. ByteLength -> (Serializer -> Eff (writer :: DV.Writer | e) Unit) -> Eff (writer :: DV.Writer | e) AB.ArrayBuffer
serialized n f = do
  s <- serializer n
  f s
  close s
