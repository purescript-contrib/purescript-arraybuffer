module Data.ArrayBuffer.Show where

import Prelude
import Data.ArrayBuffer.Types
import Data.ArrayBuffer.ArrayBuffer as AB
import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.Typed as T

-- instance showArrayView :: Show (ArrayView a) where
--   show = showImpl
--
-- instance showDataView :: Show DataView where
--   show = show <<< T.asInt8Array
--
-- instance showArrayBuffer :: Show ArrayBuffer where
--   show = show <<< DV.whole
--
-- foreign import showImpl  :: forall a. ArrayView a -> String
