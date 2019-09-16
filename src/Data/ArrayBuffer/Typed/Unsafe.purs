module Data.ArrayBuffer.Typed.Unsafe where

import Data.ArrayBuffer.Typed (class TypedArray, toString)
import Data.ArrayBuffer.Typed as TA
import Data.ArrayBuffer.Types (ArrayView)
import Data.Maybe (Maybe(..))
import Data.Generic.Rep (class Generic)
import Effect.Unsafe (unsafePerformEffect)
import Prelude (class Eq, class Monoid, class Ord, class Semigroup, class Show, bind, discard, pure, void, ($), (+), (<>))

newtype AV a t = AV (ArrayView a)

derive instance genericAV :: Generic (AV a t) _

instance ordArrayView :: (TypedArray a t, Ord t) => Ord (AV a t) where
  compare (AV a) (AV b) = unsafePerformEffect $ TA.compare a b

instance eqArrayView :: (TypedArray a t, Eq t) => Eq (AV a t) where
  eq (AV a) (AV b) = unsafePerformEffect $ TA.eq a b

instance showArrayView :: (TypedArray a t, Show t) => Show (AV a t) where
  show (AV a) = "T[" <> s <> "]"
    where s = unsafePerformEffect $ toString a

instance semigroupArrayView :: TypedArray a t => Semigroup (AV a t) where
  append (AV a) (AV b) = unsafePerformEffect do
    let la = TA.length a
        lb = TA.length b
    r <- TA.empty $ la + lb
    void $ TA.setTyped r (Just 0) a
    void $ TA.setTyped r (Just la) b
    pure $ AV r

instance monoidArrayView :: TypedArray a t => Monoid (AV a t) where
  mempty = AV $ unsafePerformEffect $ TA.empty 0
