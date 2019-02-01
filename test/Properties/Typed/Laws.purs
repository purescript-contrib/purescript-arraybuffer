module Test.Properties.Typed.Laws where

import Data.ArrayBuffer.Typed (class TypedArray)
import Data.ArrayBuffer.Typed.Gen (genFloat32, genFloat64, genInt16, genInt32, genInt8, genTypedArray, genUint16, genUint32, genUint8)
import Data.ArrayBuffer.Typed.Unsafe (AV(..))
import Data.ArrayBuffer.Types (ArrayView, Float32, Float64, Int16, Int32, Int8, Uint16, Uint32, Uint8, Uint8Clamped, kind ArrayViewType)
import Data.Float32 as F
import Data.UInt (UInt)
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Prelude (class Eq, class Monoid, class Ord, class Semigroup, Unit, discard, void, ($), (+), (<$>), (<<<))
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Laws.Data (checkEq, checkMonoid, checkOrd, checkSemigroup)
import Type.Prelude (Proxy(..))

newtype A a = A a

foreign import data ArrayElt :: ArrayViewType -> Type -> Type

class ArrayEl (a :: ArrayViewType) (t :: Type) where
  arb :: Proxy (ArrayView a) -> Gen t

instance arrayElUint8Clamped :: ArrayEl Uint8Clamped UInt where
  arb _ = genUint8
instance arrayElUint32 :: ArrayEl Uint32 UInt where
  arb _ = genUint32
instance arrayElUint16 :: ArrayEl Uint16 UInt where
  arb _ = genUint16
instance arrayElUint8 :: ArrayEl Uint8 UInt where
  arb _ = genUint8
instance arrayElInt32 :: ArrayEl Int32 Int where
  arb _ = genInt32
instance arrayElInt16 :: ArrayEl Int16 Int where
  arb _ = genInt16
instance arrayElInt8 :: ArrayEl Int8 Int where
  arb _ = genInt8
instance arrayElFloat32 :: ArrayEl Float32 F.Float32 where
  arb _ = genFloat32
instance arrayElFloat64 :: ArrayEl Float64 Number where
  arb _ = genFloat64

instance arbitraryAAV :: (TypedArray a t, ArrayEl a t) => Arbitrary (A (AV a t)) where
  arbitrary = (A <<< AV) <$> genTypedArray (arb (Proxy :: Proxy (ArrayView a)))

derive newtype instance eqA :: Eq t => Eq (A t)
derive newtype instance ordA :: Ord t => Ord (A t)
derive newtype instance semigroupA :: Semigroup t => Semigroup (A t)
derive newtype instance monoidA :: Monoid t => Monoid (A t)

typedArrayLaws :: Ref Int -> Effect Unit
typedArrayLaws count = do
  do
    let f = checkEq
    void $ Ref.modify (_ + 1) count
    f (Proxy :: Proxy (A (AV Float32 F.Float32)))
    f (Proxy :: Proxy (A (AV Float64 Number)))
    f (Proxy :: Proxy (A (AV Int16 Int)))
    f (Proxy :: Proxy (A (AV Int32 Int)))
    f (Proxy :: Proxy (A (AV Int8 Int)))
    f (Proxy :: Proxy (A (AV Uint16 UInt)))
    f (Proxy :: Proxy (A (AV Uint32 UInt)))
    f (Proxy :: Proxy (A (AV Uint8 UInt)))
    f (Proxy :: Proxy (A (AV Uint8Clamped UInt)))

  do
    let f = checkOrd
    void $ Ref.modify (_ + 1) count
    f (Proxy :: Proxy (A (AV Float32 F.Float32)))
    f (Proxy :: Proxy (A (AV Float64 Number)))
    f (Proxy :: Proxy (A (AV Int16 Int)))
    f (Proxy :: Proxy (A (AV Int32 Int)))
    f (Proxy :: Proxy (A (AV Int8 Int)))
    f (Proxy :: Proxy (A (AV Uint16 UInt)))
    f (Proxy :: Proxy (A (AV Uint32 UInt)))
    f (Proxy :: Proxy (A (AV Uint8 UInt)))
    f (Proxy :: Proxy (A (AV Uint8Clamped UInt)))

  do
    let f = checkSemigroup
    void $ Ref.modify (_ + 1) count
    f (Proxy :: Proxy (A (AV Float32 F.Float32)))
    f (Proxy :: Proxy (A (AV Float64 Number)))
    f (Proxy :: Proxy (A (AV Int16 Int)))
    f (Proxy :: Proxy (A (AV Int32 Int)))
    f (Proxy :: Proxy (A (AV Int8 Int)))
    f (Proxy :: Proxy (A (AV Uint16 UInt)))
    f (Proxy :: Proxy (A (AV Uint32 UInt)))
    f (Proxy :: Proxy (A (AV Uint8 UInt)))
    f (Proxy :: Proxy (A (AV Uint8Clamped UInt)))

  do
    let f = checkMonoid
    void $ Ref.modify (_ + 1) count
    f (Proxy :: Proxy (A (AV Float32 F.Float32)))
    f (Proxy :: Proxy (A (AV Float64 Number)))
    f (Proxy :: Proxy (A (AV Int16 Int)))
    f (Proxy :: Proxy (A (AV Int32 Int)))
    f (Proxy :: Proxy (A (AV Int8 Int)))
    f (Proxy :: Proxy (A (AV Uint16 UInt)))
    f (Proxy :: Proxy (A (AV Uint32 UInt)))
    f (Proxy :: Proxy (A (AV Uint8 UInt)))
    f (Proxy :: Proxy (A (AV Uint8Clamped UInt)))
