module Data.ArrayBuffer.DataView.Gen where

import Data.ArrayBuffer.ArrayBuffer.Gen (genArrayBuffer)
import Data.ArrayBuffer.DataView (whole, byteLength)
import Data.ArrayBuffer.Types (DataView, ByteLength, ByteOffset, kind ArrayViewType)
import Data.ArrayBuffer.ValueMapping (class BytesPerValue, class BinaryValue)

import Prelude ((<$>), bind, pure, (-), ($))
import Data.Maybe (Maybe (Just))
import Data.Vec (Vec)
import Data.Vec (fromArray) as Vec
import Data.Typelevel.Num (class Nat, toInt')
import Data.Unfoldable (replicateA)
import Control.Monad.Gen.Class (class MonadGen, chooseInt)
import Type.Proxy (Proxy (..))
import Partial.Unsafe (unsafePartial)


genDataView :: forall m
             . MonadGen m
            => ByteLength
            -> Maybe ByteLength
            -> m DataView
genDataView a b = whole <$> genArrayBuffer a b



-- | For generating some set of offsets residing inside the generated array
data WithOffsetAndValue n (a :: ArrayViewType) t = WithOffsetAndValue (Vec n ByteOffset) t DataView

genWithOffsetAndValue :: forall m n a b t
                      . MonadGen m
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
  xs <- gen
  let l = byteLength xs
  mos <- replicateA n (chooseInt 0 (l - b))
  let os = unsafePartial $ case Vec.fromArray mos of
        Just q -> q
  t <- genT
  pure (WithOffsetAndValue os t xs)
