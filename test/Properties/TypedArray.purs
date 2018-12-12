module Test.Properties.TypedArray where


import Data.ArrayBuffer.Types (ArrayView)
import Data.ArrayBuffer.Typed as TA
import Data.ArrayBuffer.Typed (class BytesPerValue, class TypedArray)
import Data.ArrayBuffer.Typed.Gen
  ( genUint8ClampedArray, genUint8Array, genUint16Array, genUint32Array
  , genInt8Array, genInt16Array, genInt32Array
  , genFloat32Array, genFloat64Array)

import Prelude
import Data.Maybe (Maybe (..))
import Data.Typelevel.Num (toInt', class Nat)
import Type.Proxy (Proxy (..))
import Test.QuickCheck (quickCheckGen, Result, (===), class Testable, class Arbitrary)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Console (log)


typedArrayTests :: Effect Unit
typedArrayTests = do
  log "    - byteLength x / bytesPerValue === length x"
  byteLengthDivBytesPerValueTests
  log "    - fromArray (toArray x) === x"
  fromArrayToArrayIsoTests
  log "    - fill y x => all (== y) x"
  allAreFilledTests


type TestableArrayF a t n q =
     Show t
  => Eq t
  => TypedArray a t
  => BytesPerValue a n
  => Arbitrary t
  => Semiring t
  => Nat n
  => ArrayView a
  -> q


overAll :: forall q. Testable q => (forall a t n. TestableArrayF a t n q) -> Effect Unit
overAll f = do
  log "      - Uint8ClampedArray"
  quickCheckGen (f <$> genUint8ClampedArray)
  log "      - Uint32Array"
  quickCheckGen (f <$> genUint32Array)
  log "      - Uint16Array"
  quickCheckGen (f <$> genUint16Array)
  log "      - Uint8Array"
  quickCheckGen (f <$> genUint8Array)
  log "      - Int32Array"
  quickCheckGen (f <$> genInt32Array)
  log "      - Int16Array"
  quickCheckGen (f <$> genInt16Array)
  log "      - Int8Array"
  quickCheckGen (f <$> genInt8Array)
  log "      - Float32Array"
  quickCheckGen (f <$> genFloat32Array)
  log "      - Float64Array"
  quickCheckGen (f <$> genFloat64Array)


byteLengthDivBytesPerValueTests :: Effect Unit
byteLengthDivBytesPerValueTests = overAll byteLengthDivBytesPerValueEqLength
  where
    byteLengthDivBytesPerValueEqLength :: forall a t n. TestableArrayF a t n Result
    byteLengthDivBytesPerValueEqLength a =
      let n = toInt' (Proxy :: Proxy n)
      in  TA.length a === (TA.byteLength a `div` n)

fromArrayToArrayIsoTests :: Effect Unit
fromArrayToArrayIsoTests = overAll fromArrayToArrayIso
  where
    fromArrayToArrayIso :: forall a t n. TestableArrayF a t n Result
    fromArrayToArrayIso x = TA.toArray (TA.fromArray (TA.toArray x) :: ArrayView a) === TA.toArray x


allAreFilledTests :: Effect Unit
allAreFilledTests = overAll allAreFilled
  where
    allAreFilled :: forall a t n. TestableArrayF a t n Result
    allAreFilled xs = unsafePerformEffect do
      let x = case TA.at xs 0 of
            Nothing -> zero
            Just y -> y
      TA.fill xs x Nothing
      b <- TA.all (\y o -> pure (y == x)) xs
      pure (b === true)


-- setSingletonIsEq 
