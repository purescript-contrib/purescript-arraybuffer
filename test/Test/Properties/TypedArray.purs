module Test.Properties.TypedArray where

import Prelude

import Control.Monad.Gen (suchThat)
import Data.Array (all, cons, drop, length, reverse, slice, snoc, sort, take, unsafeIndex) as Array
import Data.Array.Partial (head) as Array
import Data.ArrayBuffer.Typed (class TypedArray)
import Data.ArrayBuffer.Typed as TA
import Data.ArrayBuffer.Typed.Gen (WithIndices(..), genFloat32, genFloat64, genInt16, genInt32, genInt8, genTypedArray, genUint16, genUint32, genUint8, genWithIndices)
import Data.ArrayBuffer.Types (ArrayView, Float32Array, Float64Array, Int16Array, Int32Array, Int8Array, Uint16Array, Uint32Array, Uint8Array, Uint8ClampedArray)
import Data.ArrayBuffer.ValueMapping (class BytesPerType, byteWidth)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (class Testable, Result(..), quickCheckGen, (/==), (<?>), (===))
import Test.QuickCheck.Gen (Gen)
import Type.Proxy (Proxy(..))

typedArrayTests :: Ref Int -> Effect Unit
typedArrayTests count = do
  log "    - subarrayBehavesLikeArraySlice"
  subarrayBehavesLikeArraySliceTests count
  log "    - sliceBehavesLikeArraySlice"
  sliceBehavesLikeArraySliceTests count
  log "    - partBehavesLikeTakeDrop"
  partBehavesLikeTakeDropTests count
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
  withIndicesHasIndexTests count
  log "    - forall os `in` xs. all (\\o -> elem (at o xs) xs)"
  withIndicesElemTests count
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
  log "    - join \",\" x == toString x"
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
  modifyingOriginalMutatesSubArrayPartTests count
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

type TestableArrayF a t q =
  Show t
  => Eq t
  => Ord t
  => Semiring t
  => TypedArray a t
  => BytesPerType a
  => WithIndices a
  -> Effect q

overAll'
  :: forall q
   . Testable q
  => Int -- n
  -> Int -- “minimum n”?
  -> Ref Int
  -> (forall a t. TestableArrayF a t q)
  -> Effect Unit
overAll' n mn count f = do
  void (Ref.modify (_ + 1) count)

  let
    chk
      :: forall a t
       . Show t
      => Eq t
      => Ord t
      => Semiring t
      => BytesPerType a
      => TypedArray a t
      => String
      -> Int
      -> Proxy (ArrayView a)
      -> Gen t
      -> Effect Unit
    chk s n' _ gen = do
      log $ "      - " <> s
      quickCheckGen $ unsafePerformEffect <<< f <$> genWithIndices n' arr
      where
      arr :: Gen (ArrayView a)
      arr = genTypedArray gen `suchThat` \xs -> mn <= TA.length xs

  chk "Uint8ClampedArray" n (Proxy :: Proxy Uint8ClampedArray) genUint8
  chk "Uint32Array" n (Proxy :: Proxy Uint32Array) genUint32
  chk "Uint16Array" n (Proxy :: Proxy Uint16Array) genUint16
  chk "Uint8Array" n (Proxy :: Proxy Uint8Array) genUint8
  chk "Int32Array" n (Proxy :: Proxy Int32Array) genInt32
  chk "Int16Array" n (Proxy :: Proxy Int16Array) genInt16
  chk "Int8Array" n (Proxy :: Proxy Int8Array) genInt8
  chk "Float32Array" n (Proxy :: Proxy Float32Array) genFloat32
  chk "Float64Array" n (Proxy :: Proxy Float64Array) genFloat64

overAll
  :: forall q
   . Testable q
  => Int
  -> Ref Int
  -> (forall a t. TestableArrayF a t q)
  -> Effect Unit
overAll n count f = overAll' n 0 count f

overAll1
  :: forall q
   . Testable q
  => Int
  -> Ref Int
  -> (forall a t. TestableArrayF a t q)
  -> Effect Unit
overAll1 n count f = overAll' n 1 count f

subarrayBehavesLikeArraySliceTests :: Ref Int -> Effect Unit
subarrayBehavesLikeArraySliceTests count = overAll 2 count f
  where
  f :: forall a t. TestableArrayF a t Result
  f (WithIndices os xs) = do
    let
      s = unsafePartial $ os `Array.unsafeIndex` 0
      e = unsafePartial $ os `Array.unsafeIndex` 1
    axs <- TA.toArray xs
    let sxs = TA.subArray s e xs
    a <- TA.toArray sxs
    pure $ Array.slice s e axs === a

sliceBehavesLikeArraySliceTests :: Ref Int -> Effect Unit
sliceBehavesLikeArraySliceTests count = overAll 2 count f
  where
  f :: forall a t. TestableArrayF a t Result
  f (WithIndices os xs) = do
    let
      s = unsafePartial $ os `Array.unsafeIndex` 0
      e = unsafePartial $ os `Array.unsafeIndex` 1
    axs <- TA.toArray xs
    sxs <- TA.slice s e xs
    a <- TA.toArray sxs
    pure $ Array.slice s e axs === a

partBehavesLikeTakeDropTests :: Ref Int -> Effect Unit
partBehavesLikeTakeDropTests count = overAll 0 count f
  where
  f :: forall a t. TestableArrayF a t Result
  f (WithIndices _ xs) = do
    let n = 2
    axs <- TA.toArray xs
    pxs <- TA.part (TA.buffer xs) n n :: Effect (ArrayView a)
    aps <- TA.toArray pxs
    pure $ Array.take n (Array.drop n axs) === aps

byteLengthDivBytesPerValueTests :: Ref Int -> Effect Unit
byteLengthDivBytesPerValueTests count = overAll 0 count byteLengthDivBytesPerValueEqLength
  where
  byteLengthDivBytesPerValueEqLength :: forall a t. TestableArrayF a t Result
  byteLengthDivBytesPerValueEqLength (WithIndices _ xs) = do
    let b = byteWidth (Proxy :: Proxy a)
    pure $ TA.length xs === (TA.byteLength xs `div` b)

fromArrayToArrayIsoTests :: Ref Int -> Effect Unit
fromArrayToArrayIsoTests count = overAll 0 count fromArrayToArrayIso
  where
  fromArrayToArrayIso :: forall a t. TestableArrayF a t Result
  fromArrayToArrayIso (WithIndices _ xs) = do
    axs <- TA.toArray xs
    xs' <- TA.fromArray axs :: Effect (ArrayView a)
    axs' <- TA.toArray xs'
    pure $ axs' === axs

allAreFilledTests :: Ref Int -> Effect Unit
allAreFilledTests count = overAll 0 count allAreFilled
  where
  allAreFilled :: forall a t. TestableArrayF a t Result
  allAreFilled (WithIndices _ xs) = do
    e <- TA.at xs 0
    let
      x = fromMaybe zero e
      l = TA.length xs
    TA.fill x 0 l xs
    b <- TA.all (_ == x) xs
    pure (b <?> "All aren't the filled value")

setSingletonIsEqTests :: Ref Int -> Effect Unit
setSingletonIsEqTests count = overAll 1 count setSingletonIsEq
  where
  setSingletonIsEq :: forall a t. TestableArrayF a t Result
  setSingletonIsEq (WithIndices os xs) = do
    e <- TA.at xs 0
    case e of
      Nothing -> pure Success
      Just x -> do
        let o = unsafePartial $ Array.head os
        _ <- TA.set xs (Just o) [ x ]
        e' <- TA.at xs o
        pure $ e' === Just x

-- | Should work with any arbitrary predicate, but we can't generate them
allImpliesAnyTests :: Ref Int -> Effect Unit
allImpliesAnyTests count = overAll 0 count allImpliesAny
  where
  allImpliesAny :: forall a t. TestableArrayF a t Result
  allImpliesAny (WithIndices _ xs) = do
    let pred x = x /= zero
    all'' <- TA.all pred xs
    let all' = all'' <?> "All don't satisfy the predicate"
    any'' <- TA.any pred xs
    let any' = any'' <?> "None satisfy the predicate"
    pure $ (TA.length xs === zero) `xor` all' `implies` any'

-- | Should work with any arbitrary predicate, but we can't generate them
filterImpliesAllTests :: Ref Int -> Effect Unit
filterImpliesAllTests count = overAll 0 count filterImpliesAll
  where
  filterImpliesAll :: forall a t. TestableArrayF a t Result
  filterImpliesAll (WithIndices _ xs) = do
    let pred x = x /= zero
    ys <- TA.filter pred xs
    all' <- TA.all pred ys
    pure $ all' <?> "Filter doesn't imply all"

-- | Should work with any arbitrary predicate, but we can't generate them
filterIsTotalTests :: Ref Int -> Effect Unit
filterIsTotalTests count = overAll 0 count filterIsTotal
  where
  filterIsTotal :: forall a t. TestableArrayF a t Result
  filterIsTotal (WithIndices _ xs) = do
    let pred x = x /= zero
    ys <- TA.filter pred xs
    zs <- TA.filter (not pred) ys
    azs <- TA.toArray zs
    pure $ azs === []

-- | Should work with any arbitrary predicate, but we can't generate them
filterIsIdempotentTests :: Ref Int -> Effect Unit
filterIsIdempotentTests count = overAll 0 count filterIsIdempotent
  where
  filterIsIdempotent :: forall a t. TestableArrayF a t Result
  filterIsIdempotent (WithIndices _ xs) = do
    let pred x = x /= zero
    ys <- TA.filter pred xs
    zs <- TA.filter pred ys
    azs <- TA.toArray zs
    ays <- TA.toArray ys
    pure $ azs === ays

withIndicesHasIndexTests :: Ref Int -> Effect Unit
withIndicesHasIndexTests count = overAll1 5 count withIndicesHasIndex
  where
  withIndicesHasIndex :: forall a t. TestableArrayF a t Result
  withIndicesHasIndex (WithIndices os xs) = pure $
    Array.all (TA.hasIndex xs) os <?> "All doesn't have index of itself"

withIndicesElemTests :: Ref Int -> Effect Unit
withIndicesElemTests count = overAll1 5 count withIndicesElem
  where
  withIndicesElem :: forall a t. TestableArrayF a t Result
  withIndicesElem (WithIndices os xs) = do
    let fetch o = TA.at xs o
    exs <- traverse fetch os
    pure $ Array.all isJust exs <?> "All doesn't have an elem of itself"

-- | Should work with any arbitrary predicate, but we can't generate them
anyImpliesFindTests :: Ref Int -> Effect Unit
anyImpliesFindTests count = overAll 0 count anyImpliesFind
  where
  anyImpliesFind :: forall a t. TestableArrayF a t Result
  anyImpliesFind (WithIndices _ xs) = do
    let pred x = x /= zero
    a <- TA.any pred xs
    let p = a <?> "All don't satisfy the predicate"
    idx <- TA.find pred xs
    let
      q = case idx of
        Nothing -> Failed "Doesn't have a value satisfying the predicate"
        Just z ->
          if pred z then Success
          else Failed "Found value doesn't satisfy the predicate"
    pure $ p `implies` q

-- | Should work with any arbitrary predicate, but we can't generate them
findIndexImpliesAtTests :: Ref Int -> Effect Unit
findIndexImpliesAtTests count = overAll 0 count findIndexImpliesAt
  where
  findIndexImpliesAt :: forall a t. TestableArrayF a t Result
  findIndexImpliesAt (WithIndices _ xs) = do
    let pred x _ = x /= zero
    mo <- TA.findIndex pred xs
    case mo of
      Nothing -> pure Success
      Just o -> do
        e <- TA.at xs o
        case e of
          Nothing -> pure $ Failed "No value at found index"
          Just x -> pure $ pred x o <?> "Find index implies at"

indexOfImpliesAtTests :: Ref Int -> Effect Unit
indexOfImpliesAtTests count = overAll 1 count indexOfImpliesAt
  where
  indexOfImpliesAt :: forall a t. TestableArrayF a t Result
  indexOfImpliesAt (WithIndices _ xs) = do
    e <- TA.at xs 0
    case e of
      Nothing -> pure Success
      Just y -> do
        idx <- TA.indexOf y Nothing xs
        case idx of
          Nothing -> pure $ Failed "no index of"
          Just o -> do
            e' <- TA.at xs o
            pure $ e' === Just y

lastIndexOfImpliesAtTests :: Ref Int -> Effect Unit
lastIndexOfImpliesAtTests count = overAll 0 count lastIndexOfImpliesAt
  where
  lastIndexOfImpliesAt :: forall a t. TestableArrayF a t Result
  lastIndexOfImpliesAt (WithIndices _ xs) = do
    e <- TA.at xs 0
    case e of
      Nothing -> pure Success
      Just y -> do
        idx <- TA.lastIndexOf y Nothing xs
        case idx of
          Nothing -> pure $ Failed "no lastIndex of"
          Just o -> do
            e' <- TA.at xs o
            pure $ e' === Just y

foldrConsIsToArrayTests :: Ref Int -> Effect Unit
foldrConsIsToArrayTests count = overAll 0 count foldrConsIsToArray
  where
  foldrConsIsToArray :: forall a t. TestableArrayF a t Result
  foldrConsIsToArray (WithIndices _ xs) = do
    axs <- TA.toArray xs
    rxs <- TA.foldr Array.cons [] xs
    pure $ rxs === axs

foldlSnocIsToArrayTests :: Ref Int -> Effect Unit
foldlSnocIsToArrayTests count = overAll 0 count foldlSnocIsToArray
  where
  foldlSnocIsToArray :: forall a t. TestableArrayF a t Result
  foldlSnocIsToArray (WithIndices _ xs) = do
    axs <- TA.toArray xs
    rxs <- TA.foldl Array.snoc [] xs
    pure $ rxs === axs

mapIdentityIsIdentityTests :: Ref Int -> Effect Unit
mapIdentityIsIdentityTests count = overAll 0 count mapIdentityIsIdentity
  where
  mapIdentityIsIdentity :: forall a t. TestableArrayF a t Result
  mapIdentityIsIdentity (WithIndices _ xs) = do
    axs <- TA.toArray xs
    mxs <- TA.toArray (TA.map identity xs)
    pure $ axs === mxs

traverseSnocIsToArrayTests :: Ref Int -> Effect Unit
traverseSnocIsToArrayTests count = overAll 0 count traverseSnocIsToArray
  where
  traverseSnocIsToArray :: forall a t. TestableArrayF a t Result
  traverseSnocIsToArray (WithIndices _ xs) = do
    ref <- Ref.new []
    TA.traverse_ (\x -> void (Ref.modify (\xs' -> Array.snoc xs' x) ref)) xs
    ys <- Ref.read ref
    axs <- TA.toArray xs
    pure $ axs === ys

doubleReverseIsIdentityTests :: Ref Int -> Effect Unit
doubleReverseIsIdentityTests count = overAll 0 count doubleReverseIsIdentity
  where
  doubleReverseIsIdentity :: forall a t. TestableArrayF a t Result
  doubleReverseIsIdentity (WithIndices _ xs) = do
    axs <- TA.toArray xs
    TA.reverse xs
    TA.reverse xs
    axs' <- TA.toArray xs
    pure $ axs === axs'

reverseIsArrayReverseTests :: Ref Int -> Effect Unit
reverseIsArrayReverseTests count = overAll 0 count reverseIsArrayReverse
  where
  reverseIsArrayReverse :: forall a t. TestableArrayF a t Result
  reverseIsArrayReverse (WithIndices _ xs) = do
    axs <- TA.toArray xs
    TA.reverse xs
    rxs <- TA.toArray xs
    pure $ Array.reverse axs === rxs

sortIsIdempotentTests :: Ref Int -> Effect Unit
sortIsIdempotentTests count = overAll 0 count sortIsIdempotent
  where
  sortIsIdempotent :: forall a t. TestableArrayF a t Result
  sortIsIdempotent (WithIndices _ xs) = do
    TA.sort xs
    ys <- TA.toArray xs
    TA.sort xs
    zs <- TA.toArray xs
    pure $ zs === ys

sortIsArraySortTests :: Ref Int -> Effect Unit
sortIsArraySortTests count = overAll 0 count sortIsArraySort
  where
  sortIsArraySort :: forall a t. TestableArrayF a t Result
  sortIsArraySort (WithIndices _ xs) = do
    axs <- TA.toArray xs
    let ys = Array.sort axs
    TA.sort xs
    sxs <- TA.toArray xs
    pure $ sxs === ys

toStringIsJoinWithCommaTests :: Ref Int -> Effect Unit
toStringIsJoinWithCommaTests count = overAll 0 count toStringIsJoinWithComma
  where
  toStringIsJoinWithComma :: forall a t. TestableArrayF a t Result
  toStringIsJoinWithComma (WithIndices _ xs) = do
    s1 <- TA.join "," xs
    s2 <- TA.toString xs
    pure $ s1 === s2

setTypedOfSubArrayIsIdentityTests :: Ref Int -> Effect Unit
setTypedOfSubArrayIsIdentityTests count = overAll 0 count setTypedOfSubArrayIsIdentity
  where
  setTypedOfSubArrayIsIdentity :: forall a t. TestableArrayF a t Result
  setTypedOfSubArrayIsIdentity (WithIndices _ xs) = do
    ys <- TA.toArray xs
    let
      l = TA.length xs
      zsSub = TA.subArray 0 l xs
    _ <- TA.setTyped xs Nothing zsSub
    zs <- TA.toArray xs
    pure $ zs === ys

modifyingOriginalMutatesSubArrayTests :: Ref Int -> Effect Unit
modifyingOriginalMutatesSubArrayTests count = overAll 0 count modifyingOriginalMutatesSubArray
  where
  modifyingOriginalMutatesSubArray :: forall a t. TestableArrayF a t Result
  modifyingOriginalMutatesSubArray (WithIndices _ xs) = do
    axs <- TA.toArray xs
    if Array.all (eq zero) axs then pure Success
    else do
      let
        l = TA.length xs
        zsSub = TA.subArray 0 l xs
      zs <- TA.toArray zsSub
      TA.fill zero 0 l xs
      ys <- TA.toArray zsSub
      pure $ zs /== ys

modifyingSubArrayMutatesOriginalTests :: Ref Int -> Effect Unit
modifyingSubArrayMutatesOriginalTests count = overAll 0 count modifyingOriginalMutatesSubArray
  where
  modifyingOriginalMutatesSubArray :: forall a t. TestableArrayF a t Result
  modifyingOriginalMutatesSubArray (WithIndices _ xs) = do
    axs <- TA.toArray xs
    if Array.all (eq zero) axs then pure Success
    else do
      let
        l = TA.length xs
        zsSub = TA.subArray 0 l xs
      zs <- TA.toArray xs
      TA.fill zero 0 l zsSub
      ys <- TA.toArray xs
      pure $ zs /== ys

modifyingOriginalMutatesSubArrayZeroTests :: Ref Int -> Effect Unit
modifyingOriginalMutatesSubArrayZeroTests count = overAll 0 count modifyingOriginalMutatesSubArrayZero
  where
  modifyingOriginalMutatesSubArrayZero :: forall a t. TestableArrayF a t Result
  modifyingOriginalMutatesSubArrayZero (WithIndices _ xs) = do
    axs <- TA.toArray xs
    if Array.all (eq zero) axs then pure Success
    else do
      let
        l = TA.length xs
        zsSub = TA.subArray 0 l xs
      zs <- TA.toArray zsSub
      TA.fill zero 0 l xs
      ys <- TA.toArray zsSub
      pure $ zs /== ys

modifyingSubArrayMutatesOriginalZeroTests :: Ref Int -> Effect Unit
modifyingSubArrayMutatesOriginalZeroTests count = overAll 0 count modifyingSubArrayMutatesOriginalZero
  where
  modifyingSubArrayMutatesOriginalZero :: forall a t. TestableArrayF a t Result
  modifyingSubArrayMutatesOriginalZero (WithIndices _ xs) = do
    axs <- TA.toArray xs
    if Array.all (eq zero) axs then pure Success
    else do
      let
        l = TA.length xs
        zsSub = TA.subArray 0 l xs
      zs <- TA.toArray xs
      TA.fill zero 0 l zsSub
      ys <- TA.toArray xs
      pure $ zs /== ys

modifyingOriginalMutatesSubArrayAllTests :: Ref Int -> Effect Unit
modifyingOriginalMutatesSubArrayAllTests count = overAll 0 count modifyingOriginalMutatesSubArrayAll
  where
  modifyingOriginalMutatesSubArrayAll :: forall a t. TestableArrayF a t Result
  modifyingOriginalMutatesSubArrayAll (WithIndices _ xs) = do
    axs <- TA.toArray xs
    if Array.all (eq zero) axs then pure Success
    else do
      let
        l = TA.length xs
        zsSub = TA.subArray 0 l xs
      zs <- TA.toArray zsSub
      TA.fill zero 0 l xs
      ys <- TA.toArray zsSub
      pure $ zs /== ys

modifyingSubArrayMutatesOriginalAllTests :: Ref Int -> Effect Unit
modifyingSubArrayMutatesOriginalAllTests count = overAll 0 count modifyingSubArrayMutatesOriginalAll
  where
  modifyingSubArrayMutatesOriginalAll :: forall a t. TestableArrayF a t Result
  modifyingSubArrayMutatesOriginalAll (WithIndices _ xs) = do
    axs <- TA.toArray xs
    if Array.all (eq zero) axs then pure Success
    else do
      let
        l = TA.length xs
        zsSub = TA.subArray 0 l xs
      zs <- TA.toArray xs
      TA.fill zero 0 l zsSub
      ys <- TA.toArray xs
      pure $ zs /== ys

modifyingOriginalMutatesSubArrayPartTests :: Ref Int -> Effect Unit
modifyingOriginalMutatesSubArrayPartTests count = overAll 1 count modifyingOriginalMutatesSubArrayPart
  where
  modifyingOriginalMutatesSubArrayPart :: forall a t. TestableArrayF a t Result
  modifyingOriginalMutatesSubArrayPart (WithIndices os xs) = do
    let
      o = unsafePartial $ Array.head os
      l = TA.length xs
      zsSub = TA.subArray 0 l xs
    zs <- TA.toArray zsSub
    if o == 0 || Array.all (eq zero) zs then pure Success
    else do
      TA.fill zero 0 l xs
      ys <- TA.toArray zsSub
      pure $ zs /== ys

modifyingOriginalDoesntMutateSliceTests :: Ref Int -> Effect Unit
modifyingOriginalDoesntMutateSliceTests count = overAll 0 count modifyingOriginalDoesntMutateSlice
  where
  modifyingOriginalDoesntMutateSlice :: forall a t. TestableArrayF a t Result
  modifyingOriginalDoesntMutateSlice (WithIndices _ xs) = do
    axs <- TA.toArray xs
    if Array.all (eq zero) axs then pure Success
    else do
      let l = TA.length xs
      zsSub <- TA.slice 0 l xs
      zs <- TA.toArray zsSub
      TA.fill zero 0 l xs
      ys <- TA.toArray zsSub
      pure $ zs === ys

modifyingOriginalDoesntMutateSlicePartTests :: Ref Int -> Effect Unit
modifyingOriginalDoesntMutateSlicePartTests count = overAll 1 count modifyingOriginalDoesntMutateSlicePart
  where
  modifyingOriginalDoesntMutateSlicePart :: forall a t. TestableArrayF a t Result
  modifyingOriginalDoesntMutateSlicePart (WithIndices os xs) = do
    let l = TA.length xs
    axs <- TA.toArray =<< TA.slice 0 l xs
    let o = unsafePartial $ Array.head os
    e <- TA.at xs o
    if Array.all (eq zero) axs || e == Just zero then pure Success
    else do
      zsSub <- TA.slice o l xs
      zs <- TA.toArray zsSub
      TA.fill zero 0 l xs
      ys <- TA.toArray zsSub
      pure $ zs === ys

modifyingOriginalDoesntMutateSlicePart2Tests :: Ref Int -> Effect Unit
modifyingOriginalDoesntMutateSlicePart2Tests count = overAll 1 count modifyingOriginalDoesntMutateSlicePart2
  where
  modifyingOriginalDoesntMutateSlicePart2 :: forall a t. TestableArrayF a t Result
  modifyingOriginalDoesntMutateSlicePart2 (WithIndices os xs) = do
    let
      o = unsafePartial $ Array.head os
      l = TA.length xs
    axs <- TA.toArray =<< TA.slice o l xs
    e <- TA.at xs o
    if Array.all (eq zero) axs || e == Just zero then pure Success
    else do
      zsSub <- TA.slice o l xs
      zs <- TA.toArray zsSub
      TA.fill zero 0 l xs
      ys <- TA.toArray zsSub
      pure $ zs === ys

copyWithinSelfIsIdentityTests :: Ref Int -> Effect Unit
copyWithinSelfIsIdentityTests count = overAll 0 count copyWithinSelfIsIdentity
  where
  copyWithinSelfIsIdentity :: forall a t. TestableArrayF a t Result
  copyWithinSelfIsIdentity (WithIndices _ xs) = do
    ys <- TA.toArray xs
    TA.copyWithin xs 0 0 (Just (TA.length xs))
    zs <- TA.toArray xs
    pure $ zs === ys

copyWithinIsSliceTests :: Ref Int -> Effect Unit
copyWithinIsSliceTests count = overAll 1 count copyWithinIsSlice
  where
  copyWithinIsSlice :: forall a t. TestableArrayF a t Result
  copyWithinIsSlice (WithIndices os xs) = do
    let
      o = unsafePartial $ Array.head os
      l = TA.length xs
    ys <- TA.toArray =<< TA.slice o l xs
    TA.copyWithin xs 0 o Nothing
    axs <- TA.toArray xs
    zs <- pure $ Array.drop (Array.length ys) axs
    pure $ axs === ys <> zs

copyWithinViaSetTypedTests :: Ref Int -> Effect Unit
copyWithinViaSetTypedTests count = overAll 1 count copyWithinViaSetTyped
  where
  copyWithinViaSetTyped :: forall a t. TestableArrayF a t Result
  copyWithinViaSetTyped (WithIndices os xs) = do
    let o = unsafePartial $ Array.head os
    txs <- TA.toArray xs
    xs' <- TA.fromArray txs :: Effect (ArrayView a)
    let l = TA.length xs'
    ys <- TA.slice o l xs'
    _ <- TA.setTyped xs' Nothing ys
    TA.copyWithin xs 0 o Nothing
    axs <- TA.toArray xs
    axs' <- TA.toArray xs'
    pure $ axs === axs'

-- | Uses the second failure message as the result failure message
-- | https://github.com/athanclark/purescript-quickcheck-combinators/blob/293e5af07ae47b61d4eae5defef4c0f472bfa9ca/src/Test/QuickCheck/Combinators.purs#L62
implies :: Result -> Result -> Result
implies x y = case y of
  Failed y' -> case x of
    Success -> Failed ("Implied failure: " <> y')
    _ -> Success
  _ -> Success

-- | Combine two results with "Exclusive Or" logic, and with `", xor "` as the failure message separator, and "XOR" as the failure message if they are both `Success`
-- | https://github.com/athanclark/purescript-quickcheck-combinators/blob/293e5af07ae47b61d4eae5defef4c0f472bfa9ca/src/Test/QuickCheck/Combinators.purs#L44
xor :: Result -> Result -> Result
xor = xor' ", xor " "XOR"
  where
  -- Combine two results with "Exclusive Or" logic, and with a failure message separator and failure message if they are both `Success`
  xor'
    :: String -- ^ Separator
    -> String -- ^ Success failure message
    -> Result
    -> Result
    -> Result
  xor' m s x y = case Tuple x y of
    Tuple (Failed x') (Failed y') -> Failed (x' <> m <> y')
    Tuple Success Success -> Failed s
    Tuple Success y' -> y'
    Tuple x' Success -> x'
