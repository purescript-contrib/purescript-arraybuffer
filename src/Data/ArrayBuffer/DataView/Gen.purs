module Data.ArrayBuffer.DataView.Gen where

import Control.Monad.Gen (suchThat)
import Control.Monad.Gen.Class (class MonadGen, chooseInt)
import Control.Monad.Rec.Class (class MonadRec)
import Data.ArrayBuffer.ArrayBuffer.Gen (genArrayBuffer)
import Data.ArrayBuffer.DataView (whole, byteLength)
import Data.ArrayBuffer.Types (DataView, ByteOffset, ArrayViewType)
import Data.ArrayBuffer.ValueMapping (class BinaryValue, class BytesPerType, byteWidth)
import Data.Unfoldable (replicateA)
import Prelude ((<$>), bind, (<=), (-), pure)
import Type.Proxy (Proxy(..))

genDataView :: forall m. MonadGen m => m DataView
genDataView = whole <$> genArrayBuffer

-- | For generating some set of offsets residing inside the generated array, with some computable value
data WithOffsetAndValue (a :: ArrayViewType) t =
  WithOffsetAndValue (Array ByteOffset) t DataView

genWithOffsetAndValue
  :: forall m a t
   . MonadGen m
  => MonadRec m
  => BytesPerType a
  => BinaryValue a t
  => Int -- generated length
  -> m DataView -- ^ Assumes generated length is at least the minimum length of one value
  -> m t
  -> m (WithOffsetAndValue a t)
genWithOffsetAndValue n gen genT = do
  let b = byteWidth (Proxy :: Proxy a)
  xs <- gen `suchThat` \xs -> b <= byteLength xs
  let l = byteLength xs
  os <- replicateA n (chooseInt 0 (l - b))
  t <- genT
  pure (WithOffsetAndValue os t xs)
