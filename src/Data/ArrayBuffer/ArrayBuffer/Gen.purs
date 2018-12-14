module Data.ArrayBuffer.ArrayBuffer.Gen where

import Data.ArrayBuffer.Typed.Gen (genUint8Array)
import Data.ArrayBuffer.Typed (buffer)
import Data.ArrayBuffer.Types (ArrayBuffer)

import Prelude ((<$>))
import Control.Monad.Gen.Class (class MonadGen)


genArrayBuffer :: forall m. MonadGen m => m ArrayBuffer
genArrayBuffer = buffer <$> genUint8Array
