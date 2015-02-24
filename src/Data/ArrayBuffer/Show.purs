module Data.ArrayBuffer.Show where

import Data.ArrayBuffer.Types
import qualified Data.ArrayBuffer.ArrayBuffer as AB
import qualified Data.ArrayBuffer.DataView as DV
import qualified Data.ArrayBuffer.Typed as T

instance showArrayView :: Show (ArrayView a) where
  show = showImpl

instance showDataView :: Show DataView where
  show = show <<< T.asInt8Array

instance showArrayBuffer :: Show ArrayBuffer where
  show = show <<< DV.whole
  
foreign import showImpl
"""
function showImpl(a) {
  return require('util').inspect(a);
}
""" :: forall a. ArrayView a -> String

