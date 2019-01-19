module Test.Properties.TypedArray where


import Prelude

import Control.Monad.Gen (suchThat)
import Data.Array (drop, take)
import Data.Array as Array
import Data.ArrayBuffer.Typed (class TypedArray)
import Data.ArrayBuffer.Typed as TA
import Data.ArrayBuffer.Typed.Gen (WithOffset(..), genFloat32, genFloat64, genInt16, genInt32, genInt8, genTypedArray, genUint16, genUint32, genUint8, genWithOffset)
import Data.ArrayBuffer.Types (ArrayView, Float32Array, Float64Array, Int16Array, Int32Array, Int8Array, Uint16Array, Uint8Array, Uint8ClampedArray, Uint32Array)
import Data.ArrayBuffer.ValueMapping (class BytesPerValue)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (class Nat, D0, D1, D5, toInt')
import Data.Vec (head) as Vec
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (class Testable, Result(..), quickCheckGen, (/==), (<?>), (===))
import Test.QuickCheck.Combinators ((==>), (|=|))
import Test.QuickCheck.Gen (Gen)
import Type.Proxy (Proxy(..))


typedArrayTests :: Ref Int -> Effect Unit
typedArrayTests count = do
  log "    - partBehavesLikeTakeDrop"
  partBehavesLikeTakeDrop count
  log "    - byteLength x / bytesPerValue === length x"
  byteLengthDivBytesPerValueTests count
  log "    - fromArray (toArray x) === x"
  fromArrayToArrayIsoTests count
  log "    - fill y x => all (== y) x"
  allAreFilledTests count
  log "    - set x [y] o => (at x o == Just y)"
  setSingletonIsEqTests count
  log "    - all p x => any p x"
  allImpliesAnyTests count
  log "    - all p (filter p x)"
  filterImpliesAllTests count
  log "    - filter (not . p) (filter p x) == []"
  filterIsTotalTests count
  log "    - filter p (filter p x) == filter p x"
  filterIsIdempotentTests count
  log "    - forall os `in` xs. all (\\o -> hasIndex o xs)"
  withOffsetHasIndexTests count
  log "    - forall os `in` xs. all (\\o -> elem (at o xs) xs)"
  withOffsetElemTests count
  log "    - any p x => p (find p x)"
  anyImpliesFindTests count
  log "    - p (at x (findIndex p x))"
  findIndexImpliesAtTests count
  log "    - at x (indexOf y x) == y"
  indexOfImpliesAtTests count
  log "    - at x (lastIndexOf y x) == y"
  lastIndexOfImpliesAtTests count
  log "    - foldr cons [] x == toArray x"
  foldrConsIsToArrayTests count
  log "    - foldl snoc [] x == toArray x"
  foldlSnocIsToArrayTests count
  log "    - map identity x == x"
  mapIdentityIsIdentityTests count
  log "    - traverse snoc x == toArray x"
  traverseSnocIsToArrayTests count
  log "    - reverse (reverse x) == x"
  doubleReverseIsIdentityTests count
  log "    - toArray (reverse x) == Array.reverse (toArray x)"
  reverseIsArrayReverseTests count
  log "    - sort (sort x) == sort x"
  sortIsIdempotentTests count
  log "    - toArray (sort x) == Array.sort (toArray x)"
  sortIsArraySortTests count
  log "    - toString' \",\" x == toString x"
  toStringIsJoinWithCommaTests count
  log "    - setTyped x (subArray x) == x"
  setTypedOfSubArrayIsIdentityTests count
  log "    - let z' = subArray x; q = toArray z'; mutate x; pure q /= toArray z'"
  modifyingOriginalMutatesSubArrayTests count
  log "    - let z' = subArray x; q = toArray x; mutate z'; pure q /= toArray x"
  modifyingSubArrayMutatesOriginalTests count
  log "    - let z' = subArray 0 x; q = toArray z'; mutate x; pure q /= toArray z'"
  modifyingOriginalMutatesSubArrayZeroTests count
  log "    - let z' = subArray 0 x; q = toArray x; mutate z'; pure q /= toArray x"
  modifyingSubArrayMutatesOriginalZeroTests count
  log "    - let z' = subArray 0 (length x) x; q = toArray z'; mutate x; pure q /= toArray z'"
  modifyingOriginalMutatesSubArrayAllTests count
  log "    - let z' = subArray 0 (length x) x; q = toArray x; mutate z'; pure q /= toArray x"
  modifyingSubArrayMutatesOriginalAllTests count
  log "    - let z' = subArray o x; q = toArray z'; mutate x; pure q == toArray z'"
  modifyingOriginalDoesntMutateSubArrayPartTests count
  log "    - let z' = subArray 0 o x; q = toArray z'; mutate x; pure q == toArray z'"
  modifyingOriginalDoesntMutateSubArrayPart2Tests count
  log "    - let z' = slice x; q = toArray z'; mutate x; pure q == toArray z'"
  modifyingOriginalDoesntMutateSliceTests count
  log "    - let z' = slice o x; q = toArray z'; mutate x; pure q == toArray z'"
  modifyingOriginalDoesntMutateSlicePartTests count
  log "    - let z' = slice 0 o x; q = toArray z'; mutate x; pure q == toArray z'"
  modifyingOriginalDoesntMutateSlicePart2Tests count
  log "    - copyWithin x 0 0 (length x) == x"
  copyWithinSelfIsIdentityTests count
  log "    - take (o + 1) (copyWithin o x) == slice o x"
  copyWithinIsSliceTests count
  log "    - copyWithin o x == setTyped x (slice o x)"
  copyWithinViaSetTypedTests count



type TestableArrayF a b n t q =
     Show t
  => Eq t
  => Ord t
  => Semiring t
  => TypedArray a t
  => BytesPerValue a b
  => Nat b
  => WithOffset n a
  -> q


overAll :: forall q n. Testable q => Nat n => Ref Int -> (forall a b t. TestableArrayF a b n t q) -> Effect Unit
overAll count f = do
  void (Ref.modify (\x -> x + 1) count)

  let chk :: forall a b t. Show t => Eq t => Ord t => Semiring t => Nat b => BytesPerValue a b => TypedArray a t => String -> Proxy (ArrayView a) -> Gen t -> Effect Unit
      chk s _ gen = do
        log $ "      - " <> s
        quickCheckGen $ f <$> genWithOffset arr
        where arr :: Gen (ArrayView a)
              arr = genTypedArray gen `suchThat` \xs -> TA.length xs > 0

  chk "Uint8ClampedArray" (Proxy :: Proxy Uint8ClampedArray) genUint8

  chk "Uint32Array" (Proxy :: Proxy Uint32Array) genUint32

  chk "Uint16Array" (Proxy :: Proxy Uint16Array) genUint16

  chk "Uint8Array" (Proxy :: Proxy Uint8Array) genUint8

  chk "Int32Array" (Proxy :: Proxy Int32Array) genInt32

  chk "Int16Array" (Proxy :: Proxy Int16Array) genInt16

  chk "Int8Array" (Proxy :: Proxy Int8Array) genInt8

  chk "Float32Array" (Proxy :: Proxy Float32Array) genFloat32

  chk "Float64Array" (Proxy :: Proxy Float64Array) genFloat64


partBehavesLikeTakeDrop :: Ref Int -> Effect Unit
partBehavesLikeTakeDrop count = overAll count f
  where
    f :: forall a b t. TestableArrayF a b D0 t Result
    f (WithOffset _ a) =
      let n = 2
          na = TA.toArray a
          ba = TA.buffer a
          pa :: ArrayView a
          pa = TA.part ba n n
      in  take n (drop n na) === TA.toArray pa

byteLengthDivBytesPerValueTests :: Ref Int -> Effect Unit
byteLengthDivBytesPerValueTests count = overAll count byteLengthDivBytesPerValueEqLength
  where
    byteLengthDivBytesPerValueEqLength :: forall a b t. TestableArrayF a b D0 t Result
    byteLengthDivBytesPerValueEqLength (WithOffset _ a) =
      let b = toInt' (Proxy :: Proxy b)
      in  TA.length a === (TA.byteLength a `div` b)

fromArrayToArrayIsoTests :: Ref Int -> Effect Unit
fromArrayToArrayIsoTests count = overAll count fromArrayToArrayIso
  where
    fromArrayToArrayIso :: forall a b t. TestableArrayF a b D0 t Result
    fromArrayToArrayIso (WithOffset _ x) =
      TA.toArray (TA.fromArray (TA.toArray x) :: ArrayView a) === TA.toArray x


allAreFilledTests :: Ref Int -> Effect Unit
allAreFilledTests count = overAll count allAreFilled
  where
    allAreFilled :: forall a b t. TestableArrayF a b D0 t Result
    allAreFilled (WithOffset _ xs) = unsafePerformEffect do
      let x = case TA.at xs 0 of
            Nothing -> zero
            Just y -> y
      TA.fill xs x Nothing
      let b = TA.all (\y o -> y == x) xs
      pure (b <?> "All aren't the filled value")


setSingletonIsEqTests :: Ref Int -> Effect Unit
setSingletonIsEqTests count = overAll count setSingletonIsEq
  where
    setSingletonIsEq :: forall a b t. TestableArrayF a b D1 t Result
    setSingletonIsEq (WithOffset os xs) = unsafePerformEffect do
      case TA.at xs 0 of
            Nothing -> pure Success
            Just x -> do
              _ <- TA.set xs (Just (Vec.head os)) [x]
              pure (TA.at xs (Vec.head os) === Just x)


-- | Should work with any arbitrary predicate, but we can't generate them
allImpliesAnyTests :: Ref Int -> Effect Unit
allImpliesAnyTests count = overAll count allImpliesAny
  where
    allImpliesAny :: forall a b t. TestableArrayF a b D0 t Result
    allImpliesAny (WithOffset _ xs) =
      let pred x o = x /= zero
          all' = TA.all pred xs <?> "All don't satisfy the predicate"
          any' = TA.any pred xs <?> "None satisfy the predicate"
      in (TA.length xs === zero) |=| all' ==> any'


-- | Should work with any arbitrary predicate, but we can't generate them
filterImpliesAllTests :: Ref Int -> Effect Unit
filterImpliesAllTests count = overAll count filterImpliesAll
  where
    filterImpliesAll :: forall a b t. TestableArrayF a b D0 t Result
    filterImpliesAll (WithOffset _ xs) =
      let pred x o = x /= zero
          ys = TA.filter pred xs
          all' = TA.all pred ys
      in  all' <?> "Filter doesn't imply all"


-- | Should work with any arbitrary predicate, but we can't generate them
filterIsTotalTests :: Ref Int -> Effect Unit
filterIsTotalTests count = overAll count filterIsTotal
  where
    filterIsTotal :: forall a b t. TestableArrayF a b D0 t Result
    filterIsTotal (WithOffset _ xs) =
      let pred x o = x /= zero
          ys = TA.filter pred xs
          zs = TA.filter (\x o -> not pred x o) ys
      in  TA.toArray zs === []


-- | Should work with any arbitrary predicate, but we can't generate them
filterIsIdempotentTests :: Ref Int -> Effect Unit
filterIsIdempotentTests count = overAll count filterIsIdempotent
  where
    filterIsIdempotent :: forall a b t. TestableArrayF a b D0 t Result
    filterIsIdempotent (WithOffset _ xs) =
      let pred x o = x /= zero
          ys = TA.filter pred xs
          zs = TA.filter pred ys
      in  TA.toArray zs === TA.toArray ys


withOffsetHasIndexTests :: Ref Int -> Effect Unit
withOffsetHasIndexTests count = overAll count withOffsetHasIndex
  where
    withOffsetHasIndex :: forall a b t. TestableArrayF a b D5 t Result
    withOffsetHasIndex (WithOffset os xs) =
      Array.all (\o -> TA.hasIndex xs o) os <?> "All doesn't have index of itself"


withOffsetElemTests :: Ref Int -> Effect Unit
withOffsetElemTests count = overAll count withOffsetElem
  where
    withOffsetElem :: forall a b t. TestableArrayF a b D5 t Result
    withOffsetElem (WithOffset os xs) =
      Array.all (\o -> TA.elem (unsafePartial (TA.unsafeAt xs o)) Nothing xs) os
        <?> "All doesn't have an elem of itself"


-- | Should work with any arbitrary predicate, but we can't generate them
anyImpliesFindTests :: Ref Int -> Effect Unit
anyImpliesFindTests count = overAll count anyImpliesFind
  where
    anyImpliesFind :: forall a b t. TestableArrayF a b D0 t Result
    anyImpliesFind (WithOffset _ xs) =
      let pred x o = x /= zero
          p = TA.any pred xs <?> "All don't satisfy the predicate"
          q =
            case TA.find pred xs of
              Nothing -> Failed "Doesn't have a value satisfying the predicate"
              Just z -> if pred z 0
                        then Success
                        else Failed "Found value doesn't satisfy the predicate"
      in  p ==> q


-- | Should work with any arbitrary predicate, but we can't generate them
findIndexImpliesAtTests :: Ref Int -> Effect Unit
findIndexImpliesAtTests count = overAll count findIndexImpliesAt
  where
    findIndexImpliesAt :: forall a b t. TestableArrayF a b D0 t Result
    findIndexImpliesAt (WithOffset _ xs) =
      let pred x o = x /= zero
          mo = TA.findIndex pred xs
      in  case mo of
            Nothing -> Success
            Just o -> case TA.at xs o of
              Nothing -> Failed "No value at found index"
              Just x -> pred x o <?> "Find index implies at"



indexOfImpliesAtTests :: Ref Int -> Effect Unit
indexOfImpliesAtTests count = overAll count indexOfImpliesAt
  where
    indexOfImpliesAt :: forall a b t. TestableArrayF a b D0 t Result
    indexOfImpliesAt (WithOffset _ xs) =
      case TA.at xs 0 of
        Nothing -> Success
        Just y -> case TA.indexOf y Nothing xs of
          Nothing -> Failed "no index of"
          Just o -> TA.at xs o === Just y


lastIndexOfImpliesAtTests :: Ref Int -> Effect Unit
lastIndexOfImpliesAtTests count = overAll count lastIndexOfImpliesAt
  where
    lastIndexOfImpliesAt :: forall a b t. TestableArrayF a b D0 t Result
    lastIndexOfImpliesAt (WithOffset _ xs) =
      case TA.at xs 0 of
        Nothing -> Success
        Just y -> case TA.lastIndexOf y Nothing xs of
          Nothing -> Failed "no lastIndex of"
          Just o -> TA.at xs o === Just y


foldrConsIsToArrayTests :: Ref Int -> Effect Unit
foldrConsIsToArrayTests count = overAll count foldrConsIsToArray
  where
    foldrConsIsToArray :: forall a b t. TestableArrayF a b D0 t Result
    foldrConsIsToArray (WithOffset _ xs) =
      TA.foldr (\x acc _ -> Array.cons x acc) [] xs === TA.toArray xs


foldlSnocIsToArrayTests :: Ref Int -> Effect Unit
foldlSnocIsToArrayTests count = overAll count foldlSnocIsToArray
  where
    foldlSnocIsToArray :: forall a b t. TestableArrayF a b D0 t Result
    foldlSnocIsToArray (WithOffset _ xs) =
      TA.foldl (\acc x _ -> Array.snoc acc x) [] xs === TA.toArray xs


mapIdentityIsIdentityTests :: Ref Int -> Effect Unit
mapIdentityIsIdentityTests count = overAll count mapIdentityIsIdentity
  where
    mapIdentityIsIdentity :: forall a b t. TestableArrayF a b D0 t Result
    mapIdentityIsIdentity (WithOffset _ xs) =
      TA.toArray (TA.map (\x _ -> x) xs) === TA.toArray xs


traverseSnocIsToArrayTests :: Ref Int -> Effect Unit
traverseSnocIsToArrayTests count = overAll count traverseSnocIsToArray
  where
    traverseSnocIsToArray :: forall a b t. TestableArrayF a b D0 t Result
    traverseSnocIsToArray (WithOffset _ xs) =
      let ys = unsafePerformEffect do
            ref <- Ref.new []
            TA.traverse_ (\x _ -> void (Ref.modify (\xs' -> Array.snoc xs' x) ref)) xs
            Ref.read ref
      in  TA.toArray xs === ys


doubleReverseIsIdentityTests :: Ref Int -> Effect Unit
doubleReverseIsIdentityTests count = overAll count doubleReverseIsIdentity
  where
    doubleReverseIsIdentity :: forall a b t. TestableArrayF a b D0 t Result
    doubleReverseIsIdentity (WithOffset _ xs) =
      let ys = TA.toArray xs
          _ = unsafePerformEffect do
            TA.reverse xs
            TA.reverse xs
      in  TA.toArray xs === ys


reverseIsArrayReverseTests :: Ref Int -> Effect Unit
reverseIsArrayReverseTests count = overAll count reverseIsArrayReverse
  where
    reverseIsArrayReverse :: forall a b t. TestableArrayF a b D0 t Result
    reverseIsArrayReverse (WithOffset _ xs) =
      let ys = Array.reverse (TA.toArray xs)
          _ = unsafePerformEffect do
            TA.reverse xs
      in  TA.toArray xs === ys


sortIsIdempotentTests :: Ref Int -> Effect Unit
sortIsIdempotentTests count = overAll count sortIsIdempotent
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


sortIsArraySortTests :: Ref Int -> Effect Unit
sortIsArraySortTests count = overAll count sortIsArraySort
  where
    sortIsArraySort :: forall a b t. TestableArrayF a b D0 t Result
    sortIsArraySort (WithOffset _ xs) =
      let ys = Array.sort (TA.toArray xs)
          _ = unsafePerformEffect do
            TA.sort xs
      in  TA.toArray xs === ys


toStringIsJoinWithCommaTests :: Ref Int -> Effect Unit
toStringIsJoinWithCommaTests count = overAll count toStringIsJoinWithComma
  where
    toStringIsJoinWithComma :: forall a b t. TestableArrayF a b D0 t Result
    toStringIsJoinWithComma (WithOffset _ xs) =
      TA.toString' xs "," === TA.toString xs


setTypedOfSubArrayIsIdentityTests :: Ref Int -> Effect Unit
setTypedOfSubArrayIsIdentityTests count = overAll count setTypedOfSubArrayIsIdentity
  where
    setTypedOfSubArrayIsIdentity :: forall a b t. TestableArrayF a b D0 t Result
    setTypedOfSubArrayIsIdentity (WithOffset _ xs) =
      let ys = TA.toArray xs
          zsSub = TA.subArray xs Nothing
          zs = unsafePerformEffect do
            _ <- TA.setTyped xs Nothing zsSub
            pure (TA.toArray xs)
      in  zs === ys


modifyingOriginalMutatesSubArrayTests :: Ref Int -> Effect Unit
modifyingOriginalMutatesSubArrayTests count = overAll count modifyingOriginalMutatesSubArray
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


modifyingSubArrayMutatesOriginalTests :: Ref Int -> Effect Unit
modifyingSubArrayMutatesOriginalTests count = overAll count modifyingOriginalMutatesSubArray
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


modifyingOriginalMutatesSubArrayZeroTests :: Ref Int -> Effect Unit
modifyingOriginalMutatesSubArrayZeroTests count = overAll count modifyingOriginalMutatesSubArrayZero
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


modifyingSubArrayMutatesOriginalZeroTests :: Ref Int -> Effect Unit
modifyingSubArrayMutatesOriginalZeroTests count = overAll count modifyingSubArrayMutatesOriginalZero
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


modifyingOriginalMutatesSubArrayAllTests :: Ref Int -> Effect Unit
modifyingOriginalMutatesSubArrayAllTests count = overAll count modifyingOriginalMutatesSubArrayAll
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


modifyingSubArrayMutatesOriginalAllTests :: Ref Int -> Effect Unit
modifyingSubArrayMutatesOriginalAllTests count = overAll count modifyingSubArrayMutatesOriginalAll
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


modifyingOriginalDoesntMutateSubArrayPartTests :: Ref Int -> Effect Unit
modifyingOriginalDoesntMutateSubArrayPartTests count = overAll count modifyingOriginalMutatesSubArrayPart
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


modifyingOriginalDoesntMutateSubArrayPart2Tests :: Ref Int -> Effect Unit
modifyingOriginalDoesntMutateSubArrayPart2Tests count = overAll count modifyingOriginalMutatesSubArrayPart2
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


modifyingOriginalDoesntMutateSliceTests :: Ref Int -> Effect Unit
modifyingOriginalDoesntMutateSliceTests count = overAll count modifyingOriginalDoesntMutateSlice
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


modifyingOriginalDoesntMutateSlicePartTests :: Ref Int -> Effect Unit
modifyingOriginalDoesntMutateSlicePartTests count = overAll count modifyingOriginalDoesntMutateSlicePart
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


modifyingOriginalDoesntMutateSlicePart2Tests :: Ref Int -> Effect Unit
modifyingOriginalDoesntMutateSlicePart2Tests count = overAll count modifyingOriginalDoesntMutateSlicePart2
  where
    modifyingOriginalDoesntMutateSlicePart2 :: forall a b t. TestableArrayF a b D1 t Result
    modifyingOriginalDoesntMutateSlicePart2 (WithOffset os xs)
      | Array.all (eq zero) (TA.toArray (TA.slice xs (Just (Tuple (Vec.head os) Nothing)))) = Success
      | TA.at xs (Vec.head os) == Just zero = Success
      | otherwise =
        let o = Vec.head os
            zsSub = TA.slice xs (Just (Tuple 0 (Just o)))
            zs = TA.toArray zsSub
            ys = unsafePerformEffect do
              TA.fill xs zero Nothing
              pure (TA.toArray zsSub)
        in  zs === ys


copyWithinSelfIsIdentityTests :: Ref Int -> Effect Unit
copyWithinSelfIsIdentityTests count = overAll count copyWithinSelfIsIdentity
  where
    copyWithinSelfIsIdentity :: forall a b t. TestableArrayF a b D0 t Result
    copyWithinSelfIsIdentity (WithOffset _ xs) =
      let ys = TA.toArray xs
          zs = unsafePerformEffect do
            TA.copyWithin xs 0 0 (Just (TA.length xs))
            pure (TA.toArray xs)
      in  zs === ys


copyWithinIsSliceTests :: Ref Int -> Effect Unit
copyWithinIsSliceTests count = overAll count copyWithinIsSlice
  where
    copyWithinIsSlice :: forall a b t. TestableArrayF a b D1 t Result
    copyWithinIsSlice (WithOffset os xs) =
      let o = Vec.head os
          ys = TA.toArray (TA.slice xs (Just (Tuple o Nothing)))
          zs = unsafePerformEffect do
            TA.copyWithin xs 0 o Nothing
            pure $ Array.drop (Array.length ys) $ TA.toArray xs
      in  TA.toArray xs === ys <> zs


copyWithinViaSetTypedTests :: Ref Int -> Effect Unit
copyWithinViaSetTypedTests count = overAll count copyWithinViaSetTyped
  where
    copyWithinViaSetTyped :: forall a b t. TestableArrayF a b D1 t Result
    copyWithinViaSetTyped (WithOffset os xs) =
      let o = Vec.head os
          xs' = TA.fromArray (TA.toArray xs) :: ArrayView a
          _ = unsafePerformEffect do
            let ys = TA.slice xs' (Just (Tuple o Nothing))
            _ <- TA.setTyped xs' Nothing ys
            TA.copyWithin xs 0 o Nothing
      in  TA.toArray xs === TA.toArray xs'
