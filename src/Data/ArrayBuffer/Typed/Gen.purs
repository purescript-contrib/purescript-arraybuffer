-- | Functions for generating typed arrays and values.

module Data.ArrayBuffer.Typed.Gen where

import Prelude

import Control.Monad.Gen.Class (class MonadGen, sized, chooseInt, chooseFloat)
import Data.ArrayBuffer.Typed as TA
import Data.ArrayBuffer.Types (ArrayView)
import Data.ArrayBuffer.ValueMapping (class BytesPerValue)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Typelevel.Num (class Nat, toInt')
import Data.UInt (UInt)
import Data.UInt (fromInt) as UInt
import Data.UInt.Gen (genUInt) as UInt
import Data.Unfoldable (replicateA)
import Data.Vec (Vec)
import Data.Vec (fromArray) as Vec
import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy(..))


genTypedArray :: forall m a t
               . MonadGen m
              => TA.TypedArray a t
              => m t
              -> m (ArrayView a)
genTypedArray gen = sized \s -> do
  n <- chooseInt 0 s
  a <- replicateA n gen
  pure $ TA.fromArray a

genUint8 :: forall m. MonadGen m => m UInt
genUint8 = UInt.fromInt <$> chooseInt 0 255

genInt8 :: forall m. MonadGen m => m Int
genInt8 = chooseInt (-128) 127

genUint16 :: forall m. MonadGen m => m UInt
genUint16 = UInt.fromInt <$> chooseInt 0 65535

genInt16 :: forall m. MonadGen m => m Int
genInt16 = chooseInt (-32768) 32767

genUint32 :: forall m. MonadGen m => m UInt
genUint32 = UInt.genUInt bottom top

genInt32 :: forall m. MonadGen m => m Int
genInt32 = chooseInt bottom top

foreign import toFloat32 :: Number -> Number

genFloat32 :: forall m. MonadGen m => m Number
genFloat32 = toFloat32 <$> chooseFloat (-3.40282347e+38) 3.40282347e+38

genFloat64 :: forall m. MonadGen m => m Number
genFloat64 = chooseFloat ((-1.7976931348623157e+308)/div) (1.7976931348623157e+308/div)
  where div = 4.0

-- | For generating some set of offsets residing inside the generated array
data WithOffset n a = WithOffset (Vec n TA.Offset) (ArrayView a)
derive instance genericWithOffset :: Generic (ArrayView a) a' => Generic (WithOffset n a) _

genWithOffset :: forall m n b a
               . MonadGen m
              => Nat n
              => BytesPerValue a b
              => m (ArrayView a)
              -> m (WithOffset n a)
genWithOffset gen = do
  let n = toInt' (Proxy :: Proxy n)
  xs <- gen
  let l = TA.length xs
  mos <- replicateA n (chooseInt 0 (l - 1))
  let os = unsafePartial $ case Vec.fromArray mos of
        Just q -> q
  pure (WithOffset os xs)
