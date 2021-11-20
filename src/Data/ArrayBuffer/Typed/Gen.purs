-- | Functions for generating typed arrays and values.
module Data.ArrayBuffer.Typed.Gen where

import Control.Monad.Gen.Class (class MonadGen, sized, chooseInt, chooseFloat)
import Data.ArrayBuffer.Typed (class TypedArray)
import Data.ArrayBuffer.Typed as TA
import Data.ArrayBuffer.Types (ArrayView)
import Data.ArrayBuffer.ValueMapping (class BytesPerType)
import Data.Float32 (Float32, fromNumber') as F
import Data.UInt (UInt)
import Data.UInt (fromInt) as UInt
import Data.UInt.Gen (genUInt) as UInt
import Data.Unfoldable (replicateA)
import Effect.Unsafe (unsafePerformEffect)
import Prelude (bind, bottom, negate, pure, top, ($), (-), (/), (<$>))

genTypedArray
  :: forall m a t
   . MonadGen m
  => TypedArray a t
  => m t
  -> m (ArrayView a)
genTypedArray gen = sized \s -> do
  n <- chooseInt 0 s
  a <- replicateA n gen
  pure (unsafePerformEffect $ TA.fromArray a)

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

genFloat32 :: forall m. MonadGen m => m F.Float32
genFloat32 = F.fromNumber' <$> chooseFloat (-3.40282347e+38) 3.40282347e+38

genFloat64 :: forall m. MonadGen m => m Number
genFloat64 = chooseFloat ((-1.7976931348623157e+308) / div) (1.7976931348623157e+308 / div)
  where
  div = 4.0

-- | For generating some set of offsets residing inside the generated array
data WithIndices a = WithIndices (Array TA.Index) (ArrayView a)

genWithIndices
  :: forall m a
   . MonadGen m
  => BytesPerType a
  => Int -- Number of offsets residing inside the generated array
  -> m (ArrayView a)
  -> m (WithIndices a)
genWithIndices n gen = do
  xs <- gen
  let l = TA.length xs
  os <- replicateA n (chooseInt 0 (l - 1))
  pure (WithIndices os xs)
