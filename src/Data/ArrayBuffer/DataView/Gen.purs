module Data.ArrayBuffer.DataView.Gen where

import Prelude ((<$>), bind, ($), (<=), (-), pure)

import Control.Monad.Gen (suchThat)
import Control.Monad.Gen.Class (class MonadGen, chooseInt)
import Control.Monad.Rec.Class (class MonadRec)
import Data.ArrayBuffer.ArrayBuffer.Gen (genArrayBuffer)
import Data.ArrayBuffer.DataView (whole, byteLength)
import Data.ArrayBuffer.Types (DataView, ByteOffset, kind ArrayViewType)
import Data.ArrayBuffer.ValueMapping (class BytesPerValue, class BinaryValue)
import Data.Maybe (Maybe(Just))
import Data.Typelevel.Num (class Nat, toInt')
import Data.Unfoldable (replicateA)
import Data.Vec (Vec)
import Data.Vec (fromArray) as Vec
import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy(..))


genDataView :: forall m
             . MonadGen m
            => m DataView
genDataView = whole <$> genArrayBuffer



-- | For generating some set of offsets residing inside the generated array, with some computable value
data WithOffsetAndValue n (a :: ArrayViewType) t =
  WithOffsetAndValue (Vec n ByteOffset) t DataView

genWithOffsetAndValue :: forall m n a b t
                      . MonadGen m
                      => MonadRec m
                      => Nat n
                      => BytesPerValue a b
                      => BinaryValue a t
                      => Nat b
                      => m DataView -- ^ Assumes generated length is at least the minimum length of one value
                      -> m t
                      -> m (WithOffsetAndValue n a t)
genWithOffsetAndValue gen genT = do
  let n = toInt' (Proxy :: Proxy n)
      b = toInt' (Proxy :: Proxy b)
  xs <- gen `suchThat` \xs -> b <= byteLength xs
  let l = byteLength xs
  mos <- replicateA n (chooseInt 0 (l - b))
  let os = unsafePartial $ case Vec.fromArray mos of
        Just q -> q
  t <- genT
  pure (WithOffsetAndValue os t xs)
