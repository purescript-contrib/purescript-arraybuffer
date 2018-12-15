module Test.Properties.DataView where


import Data.ArrayBuffer.Types
  ( DataView
  , Uint32, Uint16, Uint8, Int32, Int16, Int8, Float32, Float64)
import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.DataView.Gen (genDataView, genWithOffsetAndValue, WithOffsetAndValue (..))
import Data.ArrayBuffer.ValueMapping (class BytesPerValue, class BinaryValue)
import Data.ArrayBuffer.Typed.Gen
  (genUWord, genWord, genUChomp, genChomp, genUByte, genByte, genFloat32, genFloat64)

import Prelude
import Data.Vec (head) as Vec
import Data.UInt (UInt)
import Data.Maybe (Maybe (..))
import Data.Typelevel.Num (class Nat, D1)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Console (log)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Test.QuickCheck (class Testable, quickCheckGen, class Arbitrary, arbitrary, Result, (===))
import Test.QuickCheck.Gen (Gen)



dataViewTests :: Ref Int -> Effect Unit
dataViewTests count = do
  pure unit


type TestableViewF a b n t q =
     Show t
  => Eq t
  => Ord t
  => Semiring t
  => Arbitrary t
  => BinaryValue a t
  => BytesPerValue a b
  => Nat b
  => WithOffsetAndValue n a t
  -> q


overAll :: forall q n. Testable q => Nat n => Ref Int -> (forall a b t. TestableViewF a b n t q) -> Effect Unit
overAll count f = do
  void (Ref.modify (\x -> x + 1) count)
  log "      - Uint32"
  quickCheckGen (f <$> (genWithOffsetAndValue (genDataView 20 Nothing) genUWord :: Gen (WithOffsetAndValue n Uint32 UInt)))
  log "      - Uint16"
  log "      - Uint8"
  log "      - Int32"
  log "      - Int16"
  log "      - Int8"
  log "      - Float32"
  log "      - Float64"


placingAValueIsThereTests :: forall a t. Ref Int -> DV.Getter a t -> DV.Setter a t -> Effect Unit
placingAValueIsThereTests count getter setter = overAll count placingAValueIsThere
  where
    placingAValueIsThere :: forall a b t. TestableViewF a b D1 t Result
    placingAValueIsThere (WithOffsetAndValue os t xs) =
      let o = Vec.head os
      in  unsafePerformEffect do
        DV.runSetter setter xs t o
        my <- DV.runGetter getter xs o
        pure (my === Just t)
