-- | Functions for generating typed arrays and values.

module Data.ArrayBuffer.Typed.Gen where

import Prelude

import Control.Monad.Gen.Class (class MonadGen, sized, chooseInt, chooseFloat)
import Data.ArrayBuffer.Typed as TA
import Data.ArrayBuffer.Types (ArrayView)
import Data.ArrayBuffer.ValueMapping (class BytesPerValue)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe)
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
              => TA.Length -- ^ Min length
              -> Maybe TA.Length -- ^ Max length
              -> m t
              -> m (ArrayView a)
genTypedArray lo mhi gen = sized \s ->
  let hi = fromMaybe s mhi
      s' = clamp lo hi s
  in  TA.fromArray <$> replicateA s' gen


genUByte :: forall m. MonadGen m => m UInt
genUByte = UInt.fromInt <$> chooseInt 0 255

genByte :: forall m. MonadGen m => m Int
genByte = chooseInt (-128) 127

genUShort :: forall m. MonadGen m => m UInt
genUShort = UInt.fromInt <$> chooseInt 0 65535

genShort :: forall m. MonadGen m => m Int
genShort = chooseInt (-32768) 32767

genUInt :: forall m. MonadGen m => m UInt
genUInt = UInt.genUInt bottom top

genInt :: forall m. MonadGen m => m Int
genInt = chooseInt bottom top

foreign import toFloat32 :: Number -> Number

genFloat32 :: forall m. MonadGen m => m Number
genFloat32 = toFloat32 <$> chooseFloat (-3.40282347e+38) 3.40282347e+38

genFloat64 :: forall m. MonadGen m => m Number
genFloat64 = chooseFloat (-1.7976931348623157e+308) 1.7976931348623157e+308

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
