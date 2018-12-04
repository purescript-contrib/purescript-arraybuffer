module Data.ArrayBuffer.Show where

import Data.ArrayBuffer.Types (ArrayView)
--
foreign import showViaInspect :: forall a. ArrayView a -> String
