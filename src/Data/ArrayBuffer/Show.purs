module Data.ArrayBuffer.Show where
       
import qualified Data.ArrayBuffer as AB
import qualified Data.ArrayBuffer.DataView as DV
import qualified Data.ArrayBuffer.Typed as T

instance showArrayView :: Show (T.ArrayView a) where
  show = showImpl

instance showDataView :: Show DV.DataView where
  show = show <<< T.asInt8Array

instance showArrayBuffer :: Show AB.ArrayBuffer where
  show = show <<< DV.whole
  
foreign import showImpl
"""
function showImpl(a) {
  return require('util').inspect(a);
}
""" :: forall a. T.ArrayView a -> String

