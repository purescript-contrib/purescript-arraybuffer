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
import Data.Typelevel.Num (class Nat, D1, D2, D4, D8)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Console (log)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Test.QuickCheck (class Testable, quickCheckGen, class Arbitrary, arbitrary, Result, (===))
import Test.QuickCheck.Gen (Gen)



dataViewTests :: Ref Int -> Effect Unit
dataViewTests count = do
  log "    - set x o => get o === Just x"
  placingAValueIsThereTests count


type TestableViewF a b n e t q =
     Show t
  => Eq t
  => Ord t
  => Semiring t
  => Arbitrary t
  => BytesPerValue a b
  => Nat b
  => DV.DataView a e t
  => WithOffsetAndValue n a e t
  -> q


overAll :: forall q n. Testable q => Nat n => Ref Int -> (forall a b e t. TestableViewF a b n e t q) -> Effect Unit
overAll count f = do
  void (Ref.modify (\x -> x + 1) count)
  log "      - Uint32 BE"
  quickCheckGen $
    let f' :: TestableViewF Uint32 D4 n DV.BE UInt q
        f' = f
    in  (f' <$> (genWithOffsetAndValue (genDataView 20 Nothing) genUWord))
  log "      - Uint32 LE"
  quickCheckGen $
    let f' :: TestableViewF Uint32 D4 n DV.LE UInt q
        f' = f
    in  (f' <$> (genWithOffsetAndValue (genDataView 20 Nothing) genUWord))
  log "      - Uint16 BE"
  quickCheckGen $
    let f' :: TestableViewF Uint16 D2 n DV.BE UInt q
        f' = f
    in  (f' <$> (genWithOffsetAndValue (genDataView 20 Nothing) genUChomp))
  log "      - Uint16 LE"
  quickCheckGen $
    let f' :: TestableViewF Uint16 D2 n DV.LE UInt q
        f' = f
    in  (f' <$> (genWithOffsetAndValue (genDataView 20 Nothing) genUChomp))
  log "      - Uint8 BE"
  quickCheckGen $
    let f' :: TestableViewF Uint8 D1 n DV.BE UInt q
        f' = f
    in  (f' <$> (genWithOffsetAndValue (genDataView 20 Nothing) genUByte))
  log "      - Uint8 LE"
  quickCheckGen $
    let f' :: TestableViewF Uint8 D1 n DV.LE UInt q
        f' = f
    in  (f' <$> (genWithOffsetAndValue (genDataView 20 Nothing) genUByte))
  log "      - Int32 BE"
  quickCheckGen $
    let f' :: TestableViewF Int32 D4 n DV.BE Int q
        f' = f
    in  (f' <$> (genWithOffsetAndValue (genDataView 20 Nothing) genWord))
  log "      - Int32 LE"
  quickCheckGen $
    let f' :: TestableViewF Int32 D4 n DV.LE Int q
        f' = f
    in  (f' <$> (genWithOffsetAndValue (genDataView 20 Nothing) genWord))
  log "      - Int16 BE"
  quickCheckGen $
    let f' :: TestableViewF Int16 D2 n DV.BE Int q
        f' = f
    in  (f' <$> (genWithOffsetAndValue (genDataView 20 Nothing) genChomp))
  log "      - Int16 LE"
  quickCheckGen $
    let f' :: TestableViewF Int16 D2 n DV.LE Int q
        f' = f
    in  (f' <$> (genWithOffsetAndValue (genDataView 20 Nothing) genChomp))
  log "      - Int8 BE"
  quickCheckGen $
    let f' :: TestableViewF Int8 D1 n DV.BE Int q
        f' = f
    in  (f' <$> (genWithOffsetAndValue (genDataView 20 Nothing) genByte))
  log "      - Int8 LE"
  quickCheckGen $
    let f' :: TestableViewF Int8 D1 n DV.LE Int q
        f' = f
    in  (f' <$> (genWithOffsetAndValue (genDataView 20 Nothing) genByte))
  log "      - Float32 BE"
  quickCheckGen $
    let f' :: TestableViewF Float32 D4 n DV.BE Number q
        f' = f
    in  (f' <$> (genWithOffsetAndValue (genDataView 20 Nothing) genFloat32))
  log "      - Float32 LE"
  quickCheckGen $
    let f' :: TestableViewF Float32 D4 n DV.LE Number q
        f' = f
    in  (f' <$> (genWithOffsetAndValue (genDataView 20 Nothing) genFloat32))
  log "      - Float64 BE"
  quickCheckGen $
    let f' :: TestableViewF Float64 D8 n DV.BE Number q
        f' = f
    in  (f' <$> (genWithOffsetAndValue (genDataView 20 Nothing) genFloat64))
  log "      - Float64 LE"
  quickCheckGen $
    let f' :: TestableViewF Float64 D8 n DV.LE Number q
        f' = f
    in  (f' <$> (genWithOffsetAndValue (genDataView 20 Nothing) genFloat64))


placingAValueIsThereTests :: Ref Int -> Effect Unit
placingAValueIsThereTests count = overAll count placingAValueIsThere
  where
    placingAValueIsThere :: forall a b e t. TestableViewF a b D1 e t Result
    placingAValueIsThere (WithOffsetAndValue os t xs) =
      let o = Vec.head os
      in  unsafePerformEffect do
        DV.set (DV.DVProxy :: DV.DVProxy a e) xs t o
        my <- DV.get (DV.DVProxy :: DV.DVProxy a e) xs o
        pure (my === Just t)
