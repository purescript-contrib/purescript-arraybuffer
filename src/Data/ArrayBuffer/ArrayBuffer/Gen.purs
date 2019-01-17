module Data.ArrayBuffer.ArrayBuffer.Gen where

import Control.Monad.Gen.Class (class MonadGen)
import Data.ArrayBuffer.Typed (buffer)
import Data.ArrayBuffer.Typed.Gen (genUByte, genTypedArray)
import Data.ArrayBuffer.Types (ArrayBuffer, ByteLength, Uint8Array)
import Data.Maybe (Maybe)
import Prelude ((<$>))


genArrayBuffer :: forall m
                . MonadGen m
               => ByteLength -- ^ Min length
               -> Maybe ByteLength -- ^ Max length
               -> m ArrayBuffer
genArrayBuffer a b = buffer <$> (genTypedArray a b genUByte :: m Uint8Array)
