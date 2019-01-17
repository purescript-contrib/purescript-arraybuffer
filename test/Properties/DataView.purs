module Test.Properties.DataView where


import Prelude

import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.DataView.Gen (genDataView, genWithOffsetAndValue, WithOffsetAndValue(..))
import Data.ArrayBuffer.Typed.Gen (genByte, genFloat32, genFloat64, genInt, genShort, genUByte, genUInt, genUShort)
import Data.ArrayBuffer.Types (Uint32, Uint16, Uint8, Int32, Int16, Int8, Float32, Float64)
import Data.ArrayBuffer.ValueMapping (class BytesPerValue)
import Data.Maybe (Maybe(..))
import Data.Typelevel.Num (class Nat, D1, D2, D4, D8)
import Data.UInt (UInt)
import Data.Vec (head) as Vec
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Test.QuickCheck (class Testable, quickCheckGen, Result, (===))



dataViewTests :: Ref Int -> Effect Unit
dataViewTests count = do
  log "    - setBE x o => getBE o === Just x"
  placingAValueIsThereTestsBE count
  log "    - setLE x o => getLE o === Just x"
  placingAValueIsThereTestsLE count


type TestableViewF a b n t q =
     Show t
  => Eq t
  => Ord t
  => Semiring t
  => BytesPerValue a b
  => Nat b
  => DV.DataView a t
  => WithOffsetAndValue n a t
  -> q


overAll :: forall q n. Testable q => Nat n => Ref Int -> (forall a b t. TestableViewF a b n t q) -> Effect Unit
overAll count f = do
  void (Ref.modify (\x -> x + 1) count)
  log "      - Uint32"
  quickCheckGen $
    let f' :: TestableViewF Uint32 D4 n UInt q
        f' = f
    in  f' <$> genWithOffsetAndValue (genDataView 20 Nothing) genUInt
  log "      - Uint16"
  quickCheckGen $
    let f' :: TestableViewF Uint16 D2 n UInt q
        f' = f
    in  f' <$> genWithOffsetAndValue (genDataView 20 Nothing) genUShort
  log "      - Uint8"
  quickCheckGen $
    let f' :: TestableViewF Uint8 D1 n UInt q
        f' = f
    in  f' <$> genWithOffsetAndValue (genDataView 20 Nothing) genUByte
  log "      - Int32"
  quickCheckGen $
    let f' :: TestableViewF Int32 D4 n Int q
        f' = f
    in  f' <$> genWithOffsetAndValue (genDataView 20 Nothing) genInt
  log "      - Int16"
  quickCheckGen $
    let f' :: TestableViewF Int16 D2 n Int q
        f' = f
    in  f' <$> genWithOffsetAndValue (genDataView 20 Nothing) genShort
  log "      - Int8"
  quickCheckGen $
    let f' :: TestableViewF Int8 D1 n Int q
        f' = f
    in  f' <$> genWithOffsetAndValue (genDataView 20 Nothing) genByte
  log "      - Float32"
  quickCheckGen $
    let f' :: TestableViewF Float32 D4 n Number q
        f' = f
    in  f' <$> genWithOffsetAndValue (genDataView 20 Nothing) genFloat32
  log "      - Float64"
  quickCheckGen $
    let f' :: TestableViewF Float64 D8 n Number q
        f' = f
    in  f' <$> genWithOffsetAndValue (genDataView 20 Nothing) genFloat64


placingAValueIsThereTestsBE :: Ref Int -> Effect Unit
placingAValueIsThereTestsBE count = overAll count placingAValueIsThere
  where
    placingAValueIsThere :: forall a b t. TestableViewF a b D1 t Result
    placingAValueIsThere (WithOffsetAndValue os t xs) =
      let o = Vec.head os
      in  unsafePerformEffect do
        _ <- DV.setBE (DV.AProxy :: DV.AProxy a) xs t o
        my <- DV.getBE (DV.AProxy :: DV.AProxy a) xs o
        pure (my === Just t)


placingAValueIsThereTestsLE :: Ref Int -> Effect Unit
placingAValueIsThereTestsLE count = overAll count placingAValueIsThere
  where
    placingAValueIsThere :: forall a b t. TestableViewF a b D1 t Result
    placingAValueIsThere (WithOffsetAndValue os t xs) =
      let o = Vec.head os
      in  unsafePerformEffect do
        _ <- DV.setLE (DV.AProxy :: DV.AProxy a) xs t o
        my <- DV.getLE (DV.AProxy :: DV.AProxy a) xs o
        pure (my === Just t)
