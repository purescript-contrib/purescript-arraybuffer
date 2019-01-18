module Data.ArrayBuffer.ArrayBuffer.Gen where

import Control.Monad.Gen.Class (class MonadGen)
import Data.ArrayBuffer.Typed (buffer)
import Data.ArrayBuffer.Typed.Gen (genTypedArray, genUint8)
import Data.ArrayBuffer.Types (ArrayBuffer, ByteLength, Uint8Array)
import Data.Maybe (Maybe)
import Prelude ((<$>))


genArrayBuffer :: forall m
                . MonadGen m
               => m ArrayBuffer
genArrayBuffer = buffer <$> (genTypedArray genUint8 :: m Uint8Array)
