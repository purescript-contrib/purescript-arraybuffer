module Test.Properties.DataView where

import Prelude

import Data.Array.Partial (head) as Array
import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.DataView.Gen (genDataView, genWithOffsetAndValue, WithOffsetAndValue(..))
import Data.ArrayBuffer.Typed.Gen (genFloat32, genFloat64, genInt16, genInt32, genInt8, genUint16, genUint32, genUint8)
import Data.ArrayBuffer.Types (Float32, Float64, Int16, Int32, Int8, Uint16, Uint32, Uint8)
import Data.ArrayBuffer.ValueMapping (class BinaryValue, class BytesPerType, class ShowArrayViewType)
import Data.Float32 (Float32) as F
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Data.UInt (UInt)
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (class Testable, quickCheckGen, Result, (===))
import Type.Proxy (Proxy(..))

dataViewTests :: Ref Int -> Effect Unit
dataViewTests count = do
  log "    - setBE x o => getBE o === Just x"
  placingAValueIsThereTests DV.BE count
  log "    - setLE x o => getLE o === Just x"
  placingAValueIsThereTests DV.LE count

type TestableViewF a name t q =
  Show t
  => Eq t
  => Ord t
  => Semiring t
  => BytesPerType a
  => BinaryValue a t
  => ShowArrayViewType a name
  => IsSymbol name
  => WithOffsetAndValue a t
  -> q

overAll
  :: forall q
   . Testable q
  => Ref Int
  -> (forall a name t. TestableViewF a name t q)
  -> Effect Unit
overAll count f = do
  void (Ref.modify (_ + 1) count)
  log "      - Uint32"
  quickCheckGen do
    let
      f' :: TestableViewF Uint32 "Uint32" UInt q
      f' = f

    f' <$> genWithOffsetAndValue 4 genDataView genUint32

  log "      - Uint16"
  quickCheckGen do
    let
      f' :: TestableViewF Uint16 "Uint16" UInt q
      f' = f

    f' <$> genWithOffsetAndValue 2 genDataView genUint16

  log "      - Uint8"
  quickCheckGen do
    let
      f' :: TestableViewF Uint8 "Uint8" UInt q
      f' = f

    f' <$> genWithOffsetAndValue 1 genDataView genUint8

  log "      - Int32"
  quickCheckGen do
    let
      f' :: TestableViewF Int32 "Int32" Int q
      f' = f

    f' <$> genWithOffsetAndValue 4 genDataView genInt32

  log "      - Int16"
  quickCheckGen do
    let
      f' :: TestableViewF Int16 "Int16" Int q
      f' = f

    f' <$> genWithOffsetAndValue 2 genDataView genInt16

  log "      - Int8"
  quickCheckGen do
    let
      f' :: TestableViewF Int8 "Int8" Int q
      f' = f

    f' <$> genWithOffsetAndValue 1 genDataView genInt8

  log "      - Float32"
  quickCheckGen do
    let
      f' :: TestableViewF Float32 "Float32" F.Float32 q
      f' = f

    f' <$> genWithOffsetAndValue 4 genDataView genFloat32

  log "      - Float64"
  quickCheckGen do
    let
      f' :: TestableViewF Float64 "Float64" Number q
      f' = f

    f' <$> genWithOffsetAndValue 8 genDataView genFloat64

placingAValueIsThereTests :: DV.Endian -> Ref Int -> Effect Unit
placingAValueIsThereTests endian count = overAll count placingAValueIsThere
  where
  placingAValueIsThere :: forall a name t. TestableViewF a name t Result
  placingAValueIsThere (WithOffsetAndValue os t xs) = do
    let
      o = unsafePartial $ Array.head os
      prx = Proxy :: Proxy a

    unsafePerformEffect do
      _ <- DV.set endian prx xs o t
      my <- DV.get endian prx xs o
      pure (my === Just t)
