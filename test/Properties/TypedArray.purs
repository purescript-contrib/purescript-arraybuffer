module Test.Properties.TypedArray where


import Data.ArrayBuffer.Types (ArrayView)
import Data.ArrayBuffer.Typed as TA
import Data.ArrayBuffer.Typed (class BytesPerValue, class TypedArray)
import Data.ArrayBuffer.Typed.Gen
  ( genUint8ClampedArray, genUint8Array, genUint16Array, genUint32Array
  , genInt8Array, genInt16Array, genInt32Array
  , genFloat32Array, genFloat64Array, WithOffset (..), genWithOffset)

import Prelude
import Data.Maybe (Maybe (..))
import Data.Tuple (Tuple (..))
import Data.Typelevel.Num (toInt', class Nat, D0, D1)
import Data.Vec (head) as Vec
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
  log "    - set x [y] o => (at x o == Just y)"
  setSingletonIsEqTests
  log "    - all p x => any p x"
  allImpliesAnyTests
  log "    - all p (filter p x)"
  filterImpliesAllTests
  log "    - filter (not . p) (filter p x) == []"
  filterIsTotalTests
  log "    - filter p (filter p x) == filter p x"
  filterIsIdempotentTests


type TestableArrayF a b n t q =
     Show t
  => Eq t
  => TypedArray a t
  => Nat b
  => BytesPerValue a b
  => Arbitrary t
  => Semiring t
  => WithOffset n a
  -> q


overAll :: forall q n. Testable q => Nat n => (forall a b t. TestableArrayF a b n t q) -> Effect Unit
overAll f = do
  log "      - Uint8ClampedArray"
  quickCheckGen (f <$> genWithOffset genUint8ClampedArray)
  log "      - Uint32Array"
  quickCheckGen (f <$> genWithOffset genUint32Array)
  log "      - Uint16Array"
  quickCheckGen (f <$> genWithOffset genUint16Array)
  log "      - Uint8Array"
  quickCheckGen (f <$> genWithOffset genUint8Array)
  log "      - Int32Array"
  quickCheckGen (f <$> genWithOffset genInt32Array)
  log "      - Int16Array"
  quickCheckGen (f <$> genWithOffset genInt16Array)
  log "      - Int8Array"
  quickCheckGen (f <$> genWithOffset genInt8Array)
  log "      - Float32Array"
  quickCheckGen (f <$> genWithOffset genFloat32Array)
  log "      - Float64Array"
  quickCheckGen (f <$> genWithOffset genFloat64Array)


byteLengthDivBytesPerValueTests :: Effect Unit
byteLengthDivBytesPerValueTests = overAll byteLengthDivBytesPerValueEqLength
  where
    byteLengthDivBytesPerValueEqLength :: forall a b t. TestableArrayF a b D0 t Result
    byteLengthDivBytesPerValueEqLength (WithOffset _ a) =
      let b = toInt' (Proxy :: Proxy b)
      in  TA.length a === (TA.byteLength a `div` b)

fromArrayToArrayIsoTests :: Effect Unit
fromArrayToArrayIsoTests = overAll fromArrayToArrayIso
  where
    fromArrayToArrayIso :: forall a b t. TestableArrayF a b D0 t Result
    fromArrayToArrayIso (WithOffset _ x) =
      TA.toArray (TA.fromArray (TA.toArray x) :: ArrayView a) === TA.toArray x


allAreFilledTests :: Effect Unit
allAreFilledTests = overAll allAreFilled
  where
    allAreFilled :: forall a b t. TestableArrayF a b D0 t Result
    allAreFilled (WithOffset _ xs) = unsafePerformEffect do
      let x = case TA.at xs 0 of
            Nothing -> zero
            Just y -> y
      TA.fill xs x Nothing
      b <- TA.all (\y o -> pure (y == x)) xs
      pure (b === true)


setSingletonIsEqTests :: Effect Unit
setSingletonIsEqTests = overAll setSingletonIsEq
  where
    setSingletonIsEq :: forall a b t. TestableArrayF a b D1 t Result
    setSingletonIsEq (WithOffset os xs) = unsafePerformEffect do
      let x = case TA.at xs 0 of
            Nothing -> zero
            Just y -> y
      TA.set xs (Just (Vec.head os)) [x]
      pure (TA.at xs (Vec.head os) === Just x)


-- | Should work with any arbitrary predicate, but we can't generate them
allImpliesAnyTests :: Effect Unit
allImpliesAnyTests = overAll allImpliesAny
  where
    allImpliesAny :: forall a b t. TestableArrayF a b D1 t Result
    allImpliesAny (WithOffset _ xs) =
      let pred x o = pure (x /= zero)
          all' = unsafePerformEffect (TA.all pred xs)
          any' = unsafePerformEffect (TA.any pred xs)
      in  (all' `implies` any') === true
    implies x y = if x == true && y == false then false else true


-- | Should work with any arbitrary predicate, but we can't generate them
filterImpliesAllTests :: Effect Unit
filterImpliesAllTests = overAll filterImpliesAll
  where
    filterImpliesAll :: forall a b t. TestableArrayF a b D1 t Result
    filterImpliesAll (WithOffset _ xs) =
      let pred x o = pure (x /= zero)
          ys = unsafePerformEffect (TA.filter pred xs)
          all' = unsafePerformEffect (TA.all pred ys)
      in  all' === true
    implies x y = if x == true && y == false then false else true


-- | Should work with any arbitrary predicate, but we can't generate them
filterIsTotalTests :: Effect Unit
filterIsTotalTests = overAll filterIsTotal
  where
    filterIsTotal :: forall a b t. TestableArrayF a b D1 t Result
    filterIsTotal (WithOffset _ xs) =
      let pred x o = pure (x /= zero)
          ys = unsafePerformEffect (TA.filter pred xs)
          zs = unsafePerformEffect (TA.filter (\x o -> not <$> pred x o) ys)
      in  TA.toArray zs === []


-- | Should work with any arbitrary predicate, but we can't generate them
filterIsIdempotentTests :: Effect Unit
filterIsIdempotentTests = overAll filterIsIdempotent
  where
    filterIsIdempotent :: forall a b t. TestableArrayF a b D1 t Result
    filterIsIdempotent (WithOffset _ xs) =
      let pred x o = pure (x /= zero)
          ys = unsafePerformEffect (TA.filter pred xs)
          zs = unsafePerformEffect (TA.filter pred ys)
      in  TA.toArray zs === TA.toArray ys

