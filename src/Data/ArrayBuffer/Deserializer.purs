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

deserializer :: forall e. AB.ArrayBuffer -> Eff (reader :: DV.Reader | e) Deserializer
deserializer ab = return $ { dv : DV.whole ab, off : 0 }

