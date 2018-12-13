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
import Data.Typelevel.Num (toInt', class Nat, D0, D1, D5)
import Data.Vec (head) as Vec
import Data.Array as Array
import Data.HeytingAlgebra (implies)
import Type.Proxy (Proxy (..))
import Test.QuickCheck (quickCheckGen, Result (..), (===), (/==), class Testable, class Arbitrary, (<?>))
import Test.QuickCheck.Combinators ((&=&), (|=|), (==>))
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Console (log)
import Effect.Ref as Ref


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
  log "    - forall os `in` xs. all (\\o -> hasIndex o xs)"
  withOffsetHasIndexTests
  log "    - forall os `in` xs. all (\\o -> elem (at o xs) xs)"
  withOffsetElemTests
  log "    - any p x => p (find p x)"
  anyImpliesFindTests
  log "    - p (at x (findIndex p x))"
  findIndexImpliesAtTests
  log "    - at x (indexOf y x) == y"
  indexOfImpliesAtTests
  log "    - at x (lastIndexOf y x) == y"
  lastIndexOfImpliesAtTests
  log "    - foldr cons [] x == toArray x"
  foldrConsIsToArrayTests
  log "    - foldl snoc [] x == toArray x"
  foldlSnocIsToArrayTests
  log "    - map identity x == x"
  mapIdentityIsIdentityTests
  log "    - traverse snoc x == toArray x"
  traverseSnocIsToArrayTests
  log "    - reverse (reverse x) == x"
  doubleReverseIsIdentityTests
  log "    - toArray (reverse x) == Array.reverse (toArray x)"
  reverseIsArrayReverseTests
  log "    - sort (sort x) == sort x"
  sortIsIdempotentTests
  log "    - toArray (sort x) == Array.sort (toArray x)"
  sortIsArraySortTests
  log "    - toString' \",\" x == toString x"
  toStringIsJoinWithCommaTests
  log "    - setTyped x (subArray x) == x"
  setTypedOfSubArrayIsIdentityTests
  log "    - let z' = subArray x; q = toArray z'; mutate x; pure q /= toArray z'"
  modifyingOriginalMutatesSubArrayTests
  log "    - let z' = subArray x; q = toArray x; mutate z'; pure q /= toArray x"
  modifyingSubArrayMutatesOriginalTests
  log "    - let z' = subArray 0 x; q = toArray z'; mutate x; pure q /= toArray z'"
  modifyingOriginalMutatesSubArrayZeroTests
  log "    - let z' = subArray 0 x; q = toArray x; mutate z'; pure q /= toArray x"
  modifyingSubArrayMutatesOriginalZeroTests
  log "    - let z' = subArray 0 (length x) x; q = toArray z'; mutate x; pure q /= toArray z'"
  modifyingOriginalMutatesSubArrayAllTests
  log "    - let z' = subArray 0 (length x) x; q = toArray x; mutate z'; pure q /= toArray x"
  modifyingSubArrayMutatesOriginalAllTests
  log "    - let z' = subArray o x; q = toArray z'; mutate x; pure q == toArray z'"
  modifyingOriginalDoesntMutateSubArrayPartTests
  log "    - let z' = subArray 0 o x; q = toArray z'; mutate x; pure q == toArray z'"
  modifyingOriginalDoesntMutateSubArrayPart2Tests
  log "    - let z' = slice x; q = toArray z'; mutate x; pure q == toArray z'"
  modifyingOriginalDoesntMutateSliceTests
  log "    - let z' = slice o x; q = toArray z'; mutate x; pure q == toArray z'"
  modifyingOriginalDoesntMutateSlicePartTests
  -- log "    - take (o + 1) (copyWithin o x) == slice o x"
  -- copyWithinIsSliceTests



type TestableArrayF a b n t q =
     Show t
  => Eq t
  => Ord t
  => Semiring t
  => Arbitrary t
  => TypedArray a t
  => BytesPerValue a b
  => Nat b
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
      pure (b <?> "All aren't the filled value")


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
    allImpliesAny :: forall a b t. TestableArrayF a b D0 t Result
    allImpliesAny (WithOffset _ xs) =
      let pred x o = pure (x /= zero)
          all' = unsafePerformEffect (TA.all pred xs) <?> "All don't satisfy the predicate"
          any' = unsafePerformEffect (TA.any pred xs) <?> "None satisfy the predicate"
      in  all' ==> any'


-- | Should work with any arbitrary predicate, but we can't generate them
filterImpliesAllTests :: Effect Unit
filterImpliesAllTests = overAll filterImpliesAll
  where
    filterImpliesAll :: forall a b t. TestableArrayF a b D0 t Result
    filterImpliesAll (WithOffset _ xs) =
      let pred x o = pure (x /= zero)
          ys = unsafePerformEffect (TA.filter pred xs)
          all' = unsafePerformEffect (TA.all pred ys)
      in  all' <?> "Filter doesn't imply all"


-- | Should work with any arbitrary predicate, but we can't generate them
filterIsTotalTests :: Effect Unit
filterIsTotalTests = overAll filterIsTotal
  where
    filterIsTotal :: forall a b t. TestableArrayF a b D0 t Result
    filterIsTotal (WithOffset _ xs) =
      let pred x o = pure (x /= zero)
          ys = unsafePerformEffect (TA.filter pred xs)
          zs = unsafePerformEffect (TA.filter (\x o -> not <$> pred x o) ys)
      in  TA.toArray zs === []


-- | Should work with any arbitrary predicate, but we can't generate them
filterIsIdempotentTests :: Effect Unit
filterIsIdempotentTests = overAll filterIsIdempotent
  where
    filterIsIdempotent :: forall a b t. TestableArrayF a b D0 t Result
    filterIsIdempotent (WithOffset _ xs) =
      let pred x o = pure (x /= zero)
          ys = unsafePerformEffect (TA.filter pred xs)
          zs = unsafePerformEffect (TA.filter pred ys)
      in  TA.toArray zs === TA.toArray ys


withOffsetHasIndexTests :: Effect Unit
withOffsetHasIndexTests = overAll withOffsetHasIndex
  where
    withOffsetHasIndex :: forall a b t. TestableArrayF a b D5 t Result
    withOffsetHasIndex (WithOffset os xs) =
      Array.all (\o -> TA.hasIndex xs o) os <?> "All doesn't have index of itself"


withOffsetElemTests :: Effect Unit
withOffsetElemTests = overAll withOffsetElem
  where
    withOffsetElem :: forall a b t. TestableArrayF a b D5 t Result
    withOffsetElem (WithOffset os xs) =
      Array.all (\o -> TA.elem (unsafePerformEffect (TA.unsafeAt xs o)) Nothing xs) os
        <?> "All doesn't have an elem of itself"


-- | Should work with any arbitrary predicate, but we can't generate them
anyImpliesFindTests :: Effect Unit
anyImpliesFindTests = overAll anyImpliesFind
  where
    anyImpliesFind :: forall a b t. TestableArrayF a b D0 t Result
    anyImpliesFind (WithOffset _ xs) =
      let pred x o = pure (x /= zero)
          p = unsafePerformEffect (TA.any pred xs) <?> "All don't satisfy the predicate"
          q = unsafePerformEffect do
            mzs <- TA.find xs pred
            case mzs of
              Nothing -> pure (Failed "Doesn't have a value satisfying the predicate")
              Just z -> do
                b <- pred z 0
                pure $
                  if b
                    then Success
                    else Failed "Found value doesn't satisfy the predicate"
      in  p ==> q


-- | Should work with any arbitrary predicate, but we can't generate them
findIndexImpliesAtTests :: Effect Unit
findIndexImpliesAtTests = overAll findIndexImpliesAt
  where
    findIndexImpliesAt :: forall a b t. TestableArrayF a b D0 t Result
    findIndexImpliesAt (WithOffset _ xs) =
      let pred x o = pure (x /= zero)
          mo = unsafePerformEffect (TA.findIndex xs pred)
      in  case mo of
            Nothing -> Success
            Just o -> case TA.at xs o of
              Nothing -> Failed "No value at found index"
              Just x -> unsafePerformEffect (pred x o) <?> "Find index implies at"



indexOfImpliesAtTests :: Effect Unit
indexOfImpliesAtTests = overAll indexOfImpliesAt
  where
    indexOfImpliesAt :: forall a b t. TestableArrayF a b D0 t Result
    indexOfImpliesAt (WithOffset _ xs) =
      case TA.at xs 0 of
        Nothing -> Success
        Just y -> case TA.indexOf xs y Nothing of
          Nothing -> Failed "no index of"
          Just o -> TA.at xs o === Just y


lastIndexOfImpliesAtTests :: Effect Unit
lastIndexOfImpliesAtTests = overAll lastIndexOfImpliesAt
  where
    lastIndexOfImpliesAt :: forall a b t. TestableArrayF a b D0 t Result
    lastIndexOfImpliesAt (WithOffset _ xs) =
      case TA.at xs 0 of
        Nothing -> Success
        Just y -> case TA.lastIndexOf xs y Nothing of
          Nothing -> Failed "no lastIndex of"
          Just o -> TA.at xs o === Just y


foldrConsIsToArrayTests :: Effect Unit
foldrConsIsToArrayTests = overAll foldrConsIsToArray
  where
    foldrConsIsToArray :: forall a b t. TestableArrayF a b D0 t Result
    foldrConsIsToArray (WithOffset _ xs) =
      TA.foldr xs (\x acc _ -> Array.cons x acc) [] === TA.toArray xs


foldlSnocIsToArrayTests :: Effect Unit
foldlSnocIsToArrayTests = overAll foldlSnocIsToArray
  where
    foldlSnocIsToArray :: forall a b t. TestableArrayF a b D0 t Result
    foldlSnocIsToArray (WithOffset _ xs) =
      TA.foldl xs (\acc x _ -> Array.snoc acc x) [] === TA.toArray xs


mapIdentityIsIdentityTests :: Effect Unit
mapIdentityIsIdentityTests = overAll mapIdentityIsIdentity
  where
    mapIdentityIsIdentity :: forall a b t. TestableArrayF a b D0 t Result
    mapIdentityIsIdentity (WithOffset _ xs) =
      TA.toArray (TA.map (\x _ -> x) xs) === TA.toArray xs


traverseSnocIsToArrayTests :: Effect Unit
traverseSnocIsToArrayTests = overAll traverseSnocIsToArray
  where
    traverseSnocIsToArray :: forall a b t. TestableArrayF a b D0 t Result
    traverseSnocIsToArray (WithOffset _ xs) =
      let ys = unsafePerformEffect do
            ref <- Ref.new []
            TA.traverse_ (\x _ -> void (Ref.modify (\xs' -> Array.snoc xs' x) ref)) xs
            Ref.read ref
      in  TA.toArray xs === ys


doubleReverseIsIdentityTests :: Effect Unit
doubleReverseIsIdentityTests = overAll doubleReverseIsIdentity
  where
    doubleReverseIsIdentity :: forall a b t. TestableArrayF a b D0 t Result
    doubleReverseIsIdentity (WithOffset _ xs) =
      let ys = TA.toArray xs
          _ = unsafePerformEffect do
            TA.reverse xs
            TA.reverse xs
      in  TA.toArray xs === ys


reverseIsArrayReverseTests :: Effect Unit
reverseIsArrayReverseTests = overAll reverseIsArrayReverse
  where
    reverseIsArrayReverse :: forall a b t. TestableArrayF a b D0 t Result
    reverseIsArrayReverse (WithOffset _ xs) =
      let ys = Array.reverse (TA.toArray xs)
          _ = unsafePerformEffect do
            TA.reverse xs
      in  TA.toArray xs === ys


sortIsIdempotentTests :: Effect Unit
sortIsIdempotentTests = overAll sortIsIdempotent
  where
    sortIsIdempotent :: forall a b t. TestableArrayF a b D0 t Result
    sortIsIdempotent (WithOffset _ xs) =
      let ys = unsafePerformEffect do
            TA.sort xs
            pure (TA.toArray xs)
          zs = unsafePerformEffect do
            TA.sort xs
            pure (TA.toArray xs)
      in  zs === ys


sortIsArraySortTests :: Effect Unit
sortIsArraySortTests = overAll sortIsArraySort
  where
    sortIsArraySort :: forall a b t. TestableArrayF a b D0 t Result
    sortIsArraySort (WithOffset _ xs) =
      let ys = Array.sort (TA.toArray xs)
          _ = unsafePerformEffect do
            TA.sort xs
      in  TA.toArray xs === ys


toStringIsJoinWithCommaTests :: Effect Unit
toStringIsJoinWithCommaTests = overAll toStringIsJoinWithComma
  where
    toStringIsJoinWithComma :: forall a b t. TestableArrayF a b D0 t Result
    toStringIsJoinWithComma (WithOffset _ xs) =
      TA.toString' xs "," === TA.toString xs


setTypedOfSubArrayIsIdentityTests :: Effect Unit
setTypedOfSubArrayIsIdentityTests = overAll setTypedOfSubArrayIsIdentity
  where
    setTypedOfSubArrayIsIdentity :: forall a b t. TestableArrayF a b D0 t Result
    setTypedOfSubArrayIsIdentity (WithOffset _ xs) =
      let ys = TA.toArray xs
          zsSub = TA.subArray xs Nothing
          zs = unsafePerformEffect do
            TA.setTyped xs Nothing zsSub
            pure (TA.toArray xs)
      in  zs === ys


-- setTyped of subArray is copyWithin


modifyingOriginalMutatesSubArrayTests :: Effect Unit
modifyingOriginalMutatesSubArrayTests = overAll modifyingOriginalMutatesSubArray
  where
    modifyingOriginalMutatesSubArray :: forall a b t. TestableArrayF a b D0 t Result
    modifyingOriginalMutatesSubArray (WithOffset _ xs)
      | Array.all (eq zero) (TA.toArray xs) = Success
      | otherwise =
        let zsSub = TA.subArray xs Nothing
            zs = TA.toArray zsSub
            ys = unsafePerformEffect do
              TA.fill xs zero Nothing
              pure (TA.toArray zsSub)
        in  zs /== ys


modifyingSubArrayMutatesOriginalTests :: Effect Unit
modifyingSubArrayMutatesOriginalTests = overAll modifyingOriginalMutatesSubArray
  where
    modifyingOriginalMutatesSubArray :: forall a b t. TestableArrayF a b D0 t Result
    modifyingOriginalMutatesSubArray (WithOffset _ xs)
      | Array.all (eq zero) (TA.toArray xs) = Success
      | otherwise =
        let zsSub = TA.subArray xs Nothing
            zs = TA.toArray xs
            ys = unsafePerformEffect do
              TA.fill zsSub zero Nothing
              pure (TA.toArray xs)
        in  zs /== ys


modifyingOriginalMutatesSubArrayZeroTests :: Effect Unit
modifyingOriginalMutatesSubArrayZeroTests = overAll modifyingOriginalMutatesSubArrayZero
  where
    modifyingOriginalMutatesSubArrayZero :: forall a b t. TestableArrayF a b D0 t Result
    modifyingOriginalMutatesSubArrayZero (WithOffset _ xs)
      | Array.all (eq zero) (TA.toArray xs) = Success
      | otherwise =
        let zsSub = TA.subArray xs (Just (Tuple 0 Nothing))
            zs = TA.toArray zsSub
            ys = unsafePerformEffect do
              TA.fill xs zero Nothing
              pure (TA.toArray zsSub)
        in  zs /== ys


modifyingSubArrayMutatesOriginalZeroTests :: Effect Unit
modifyingSubArrayMutatesOriginalZeroTests = overAll modifyingSubArrayMutatesOriginalZero
  where
    modifyingSubArrayMutatesOriginalZero :: forall a b t. TestableArrayF a b D0 t Result
    modifyingSubArrayMutatesOriginalZero (WithOffset _ xs)
      | Array.all (eq zero) (TA.toArray xs) = Success
      | otherwise =
        let zsSub = TA.subArray xs (Just (Tuple 0 Nothing))
            zs = TA.toArray xs
            ys = unsafePerformEffect do
              TA.fill zsSub zero Nothing
              pure (TA.toArray xs)
        in  zs /== ys


modifyingOriginalMutatesSubArrayAllTests :: Effect Unit
modifyingOriginalMutatesSubArrayAllTests = overAll modifyingOriginalMutatesSubArrayAll
  where
    modifyingOriginalMutatesSubArrayAll :: forall a b t. TestableArrayF a b D0 t Result
    modifyingOriginalMutatesSubArrayAll (WithOffset _ xs)
      | Array.all (eq zero) (TA.toArray xs) = Success
      | otherwise =
        let zsSub = TA.subArray xs (Just (Tuple 0 (Just (TA.length xs))))
            zs = TA.toArray zsSub
            ys = unsafePerformEffect do
              TA.fill xs zero Nothing
              pure (TA.toArray zsSub)
        in  zs /== ys


modifyingSubArrayMutatesOriginalAllTests :: Effect Unit
modifyingSubArrayMutatesOriginalAllTests = overAll modifyingSubArrayMutatesOriginalAll
  where
    modifyingSubArrayMutatesOriginalAll :: forall a b t. TestableArrayF a b D0 t Result
    modifyingSubArrayMutatesOriginalAll (WithOffset _ xs)
      | Array.all (eq zero) (TA.toArray xs) = Success
      | otherwise =
        let zsSub = TA.subArray xs (Just (Tuple 0 (Just (TA.length xs))))
            zs = TA.toArray xs
            ys = unsafePerformEffect do
              TA.fill zsSub zero Nothing
              pure (TA.toArray xs)
        in  zs /== ys


modifyingOriginalDoesntMutateSubArrayPartTests :: Effect Unit
modifyingOriginalDoesntMutateSubArrayPartTests = overAll modifyingOriginalMutatesSubArrayPart
  where
    modifyingOriginalMutatesSubArrayPart :: forall a b t. TestableArrayF a b D1 t Result
    modifyingOriginalMutatesSubArrayPart (WithOffset os xs)
      | Vec.head os == 0 = Success
      | Array.all (eq zero) (TA.toArray (TA.subArray xs Nothing)) = Success
      | TA.at xs (Vec.head os) == Just zero = Success
      | otherwise =
        let o = Vec.head os
            zsSub = TA.subArray xs (Just (Tuple o Nothing))
            zs = TA.toArray zsSub
            ys = unsafePerformEffect do
              TA.fill xs zero Nothing
              pure (TA.toArray zsSub)
        in  zs === ys


modifyingOriginalDoesntMutateSubArrayPart2Tests :: Effect Unit
modifyingOriginalDoesntMutateSubArrayPart2Tests = overAll modifyingOriginalMutatesSubArrayPart2
  where
    modifyingOriginalMutatesSubArrayPart2 :: forall a b t. TestableArrayF a b D1 t Result
    modifyingOriginalMutatesSubArrayPart2 (WithOffset os xs)
      | Vec.head os == 0 = Success
      | Array.all (eq zero) (TA.toArray (TA.subArray xs Nothing)) = Success
      | TA.at xs (Vec.head os) == Just zero = Success
      | otherwise =
        let o = Vec.head os
            zsSub = TA.subArray xs (Just (Tuple 0 (Just o)))
            zs = TA.toArray zsSub
            ys = unsafePerformEffect do
              TA.fill xs zero Nothing
              pure (TA.toArray zsSub)
        in  zs === ys


modifyingOriginalDoesntMutateSliceTests :: Effect Unit
modifyingOriginalDoesntMutateSliceTests = overAll modifyingOriginalDoesntMutateSlice
  where
    modifyingOriginalDoesntMutateSlice :: forall a b t. TestableArrayF a b D0 t Result
    modifyingOriginalDoesntMutateSlice (WithOffset _ xs)
      | Array.all (eq zero) (TA.toArray xs) = Success
      | otherwise =
        let zsSub = TA.slice xs Nothing
            zs = TA.toArray zsSub
            ys = unsafePerformEffect do
              TA.fill xs zero Nothing
              pure (TA.toArray zsSub)
        in  zs === ys


modifyingOriginalDoesntMutateSlicePartTests :: Effect Unit
modifyingOriginalDoesntMutateSlicePartTests = overAll modifyingOriginalDoesntMutateSlicePart
  where
    modifyingOriginalDoesntMutateSlicePart :: forall a b t. TestableArrayF a b D1 t Result
    modifyingOriginalDoesntMutateSlicePart (WithOffset os xs)
      | Array.all (eq zero) (TA.toArray (TA.slice xs (Just (Tuple (Vec.head os) Nothing)))) = Success
      | TA.at xs (Vec.head os) == Just zero = Success
      | otherwise =
        let o = Vec.head os
            zsSub = TA.slice xs (Just (Tuple o Nothing))
            zs = TA.toArray zsSub
            ys = unsafePerformEffect do
              TA.fill xs zero Nothing
              pure (TA.toArray zsSub)
        in  zs === ys


copyWithinIsSliceTests :: Effect Unit
copyWithinIsSliceTests = overAll copyWithinIsSlice
  where
    copyWithinIsSlice :: forall a b t. TestableArrayF a b D1 t Result
    copyWithinIsSlice (WithOffset os xs) =
      let o = Vec.head os
          ys = TA.toArray (TA.slice xs (Just (Tuple o Nothing)))
          zs = unsafePerformEffect do
            TA.copyWithin xs 0 o Nothing
            pure $ Array.take (Array.length ys - o) $ TA.toArray xs
      in  zs === ys




-- TODO: copyWithin, setTyped, slice, subArray
