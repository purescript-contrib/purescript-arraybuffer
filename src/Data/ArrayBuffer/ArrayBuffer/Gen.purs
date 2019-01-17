module Data.ArrayBuffer.ArrayBuffer.Gen where

import Data.ArrayBuffer.Typed.Gen (genUByte, genTypedArray)
import Data.ArrayBuffer.Typed (buffer)
import Data.ArrayBuffer.Types (ArrayBuffer, ByteLength, Uint8Array)

import Prelude ((<$>))
import Data.Maybe (Maybe)
import Control.Monad.Gen.Class (class MonadGen)


genArrayBuffer :: forall m
                . MonadGen m
               => ByteLength -- ^ Min length
               -> Maybe ByteLength -- ^ Max length
               -> m ArrayBuffer
genArrayBuffer a b = buffer <$> (genTypedArray a b genUByte :: m Uint8Array)
