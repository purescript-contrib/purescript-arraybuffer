module Data.ArrayBuffer.DataView.Gen where

import Data.ArrayBuffer.ArrayBuffer.Gen (genArrayBuffer)
import Data.ArrayBuffer.DataView (whole)
import Data.ArrayBuffer.Types (DataView, ByteLength)

import Prelude ((<$>))
import Data.Maybe (Maybe)
import Control.Monad.Gen.Class (class MonadGen)


genDataView :: forall m
             . MonadGen m
            => ByteLength
            -> Maybe ByteLength
            -> m DataView
genDataView a b = whole <$> genArrayBuffer a b
