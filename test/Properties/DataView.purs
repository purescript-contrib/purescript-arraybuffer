module Test.Properties.DataView where


import Prelude

import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.DataView.Gen (genDataView, genWithOffsetAndValue, WithOffsetAndValue(..))
import Data.ArrayBuffer.Typed.Gen (genFloat32, genFloat64, genInt16, genInt32, genInt8, genUint16, genUint32, genUint8)
import Data.ArrayBuffer.Types (Float32, Float64, Int16, Int32, Int8, Uint16, Uint32, Uint8)
import Data.ArrayBuffer.ValueMapping (class BytesPerValue)
import Data.Maybe (Maybe(..))
import Data.Typelevel.Num (class Nat, D1, D2, D4, D8)
import Data.UInt (UInt)
import Data.Float32 (Float32) as F
import Data.Vec (head) as Vec
import Data.Symbol (class IsSymbol)
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Test.QuickCheck (class Testable, quickCheckGen, Result, (===))



dataViewTests :: Ref Int -> Effect Unit
dataViewTests count = do
  log "    - setBE x o => getBE o === Just x"
  placingAValueIsThereTests DV.BE count
  log "    - setLE x o => getLE o === Just x"
  placingAValueIsThereTests DV.LE count


type TestableViewF a name b n t q =
     Show t
  => Eq t
  => Ord t
  => Semiring t
  => BytesPerValue a b
  => DV.ShowArrayViewType a name
  => IsSymbol name
  => Nat b
  => DV.DataView a t
  => WithOffsetAndValue n a t
  -> q


overAll :: forall q n. Testable q => Nat n => Ref Int -> (forall a name b t. TestableViewF a name b n t q) -> Effect Unit
overAll count f = do
  void (Ref.modify (_ + 1) count)
  log "      - Uint32"
  quickCheckGen $
    let f' :: TestableViewF Uint32 "Uint32" D4 n UInt q
        f' = f
    in  f' <$> genWithOffsetAndValue genDataView genUint32

  log "      - Uint16"
  quickCheckGen $
    let f' :: TestableViewF Uint16 "Uint16" D2 n UInt q
        f' = f
    in  f' <$> genWithOffsetAndValue genDataView genUint16

  log "      - Uint8"
  quickCheckGen $
    let f' :: TestableViewF Uint8 "Uint8" D1 n UInt q
        f' = f
    in  f' <$> genWithOffsetAndValue genDataView genUint8

  log "      - Int32"
  quickCheckGen $
    let f' :: TestableViewF Int32 "Int32" D4 n Int q
        f' = f
    in  f' <$> genWithOffsetAndValue genDataView genInt32

  log "      - Int16"
  quickCheckGen $
    let f' :: TestableViewF Int16 "Int16" D2 n Int q
        f' = f
    in  f' <$> genWithOffsetAndValue genDataView genInt16

  log "      - Int8"
  quickCheckGen $
    let f' :: TestableViewF Int8 "Int8" D1 n Int q
        f' = f
    in  f' <$> genWithOffsetAndValue genDataView genInt8

  log "      - Float32"
  quickCheckGen $
    let f' :: TestableViewF Float32 "Float32" D4 n F.Float32 q
        f' = f
    in  f' <$> genWithOffsetAndValue genDataView genFloat32

  log "      - Float64"
  quickCheckGen $
    let f' :: TestableViewF Float64 "Float64" D8 n Number q
        f' = f
    in  f' <$> genWithOffsetAndValue genDataView genFloat64


placingAValueIsThereTests :: DV.Endian -> Ref Int -> Effect Unit
placingAValueIsThereTests endian count = overAll count placingAValueIsThere
  where
    placingAValueIsThere :: forall a name b t. TestableViewF a name b D1 t Result
    placingAValueIsThere (WithOffsetAndValue os t xs) =
      let o = Vec.head os
          prx = DV.AProxy :: DV.AProxy a
      in  unsafePerformEffect do
        _ <- DV.set endian prx xs o t
        my <- DV.get endian prx xs o
        pure (my === Just t)
