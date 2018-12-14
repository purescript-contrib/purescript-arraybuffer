module Data.ArrayBuffer.DataView.Gen where

import Data.ArrayBuffer.ArrayBuffer.Gen (genArrayBuffer)
import Data.ArrayBuffer.DataView (whole)
import Data.ArrayBuffer.Types (DataView)

import Prelude ((<$>))
import Control.Monad.Gen.Class (class MonadGen)


genDataView :: forall m. MonadGen m => m DataView
genDataView = whole <$> genArrayBuffer
