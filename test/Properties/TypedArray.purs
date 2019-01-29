module Test.Properties.TypedArray where


import Prelude

import Control.Monad.Gen (suchThat)
import Data.Array as Array
import Data.ArrayBuffer.Typed (class TypedArray)
import Data.ArrayBuffer.Typed as TA
import Data.ArrayBuffer.Typed.Gen (WithOffset(..), genFloat32, genFloat64, genInt16, genInt32, genInt8, genTypedArray, genUint16, genUint32, genUint8, genWithOffset)
import Data.ArrayBuffer.Types (ArrayView, Float32Array, Float64Array, Int16Array, Int32Array, Int8Array, Uint16Array, Uint8Array, Uint8ClampedArray, Uint32Array)
import Data.ArrayBuffer.ValueMapping (class BytesPerValue)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Typelevel.Num (class Nat, D0, D1, D2, D5, d0, d1, toInt')
import Data.Vec (head, index) as Vec
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



type TestableArrayF a b n t q =
     Show t
  => Eq t
  => Ord t
  => Semiring t
  => TypedArray a t
  => BytesPerValue a b
  => Nat b
  => WithOffset n a
  -> Effect q


overAll' :: forall q n. Testable q => Nat n => Int -> Ref Int -> (forall a b t. TestableArrayF a b n t q) -> Effect Unit
overAll' mn count f = do
  void (Ref.modify (_ + 1) count)

  let chk :: forall a b t. Show t => Eq t => Ord t => Semiring t => Nat b => BytesPerValue a b => TypedArray a t => String -> Proxy (ArrayView a) -> Gen t -> Effect Unit
      chk s _ gen = do
        log $ "      - " <> s
        quickCheckGen $ unsafePerformEffect <<< f <$> genWithOffset arr
        where arr :: Gen (ArrayView a)
              arr = genTypedArray gen `suchThat` \xs -> mn <= TA.length xs

  chk "Uint8ClampedArray" (Proxy :: Proxy Uint8ClampedArray) genUint8
  chk "Uint32Array" (Proxy :: Proxy Uint32Array) genUint32
  chk "Uint16Array" (Proxy :: Proxy Uint16Array) genUint16
  chk "Uint8Array" (Proxy :: Proxy Uint8Array) genUint8
  chk "Int32Array" (Proxy :: Proxy Int32Array) genInt32
  chk "Int16Array" (Proxy :: Proxy Int16Array) genInt16
  chk "Int8Array" (Proxy :: Proxy Int8Array) genInt8
  chk "Float32Array" (Proxy :: Proxy Float32Array) genFloat32
  chk "Float64Array" (Proxy :: Proxy Float64Array) genFloat64


overAll :: forall q n. Testable q => Nat n => Ref Int -> (forall a b t. TestableArrayF a b n t q) -> Effect Unit
overAll count f = overAll' 0 count f

overAll1 :: forall q n. Testable q => Nat n => Ref Int -> (forall a b t. TestableArrayF a b n t q) -> Effect Unit
overAll1 count f = overAll' 1 count f

subarrayBehavesLikeArraySliceTests :: Ref Int -> Effect Unit
subarrayBehavesLikeArraySliceTests count = overAll count f
  where
    f :: forall a b t. TestableArrayF a b D2 t Result
    f (WithOffset os xs) = do
      let s = os `Vec.index` d0
          e = os `Vec.index` d1
      axs <- TA.toArray xs
      let sxs = TA.subArray s e xs
      a <- TA.toArray sxs
      pure $ Array.slice s e axs === a

sliceBehavesLikeArraySliceTests :: Ref Int -> Effect Unit
sliceBehavesLikeArraySliceTests count = overAll count f
  where
    f :: forall a b t. TestableArrayF a b D2 t Result
    f (WithOffset os xs) = do
      let s = os `Vec.index` d0
          e = os `Vec.index` d1
      axs <- TA.toArray xs
      sxs <- TA.slice s e xs
      a <- TA.toArray sxs
      pure $ Array.slice s e axs === a

partBehavesLikeTakeDropTests :: Ref Int -> Effect Unit
partBehavesLikeTakeDropTests count = overAll count f
  where
    f :: forall a b t. TestableArrayF a b D0 t Result
    f (WithOffset _ xs) = do
      let n = 2
      axs <- TA.toArray xs
      pxs <- TA.part (TA.buffer xs) n n :: Effect (ArrayView a)
      aps <- TA.toArray pxs
      pure $ Array.take n (Array.drop n axs) === aps

byteLengthDivBytesPerValueTests :: Ref Int -> Effect Unit
byteLengthDivBytesPerValueTests count = overAll count byteLengthDivBytesPerValueEqLength
  where
    byteLengthDivBytesPerValueEqLength :: forall a b t. TestableArrayF a b D0 t Result
    byteLengthDivBytesPerValueEqLength (WithOffset _ a) =
      let b = toInt' (Proxy :: Proxy b)
      in  pure $ TA.length a === (TA.byteLength a `div` b)

fromArrayToArrayIsoTests :: Ref Int -> Effect Unit
fromArrayToArrayIsoTests count = overAll count fromArrayToArrayIso
  where
    fromArrayToArrayIso :: forall a b t. TestableArrayF a b D0 t Result
    fromArrayToArrayIso (WithOffset _ xs) = do
      axs <- TA.toArray xs
      xs' <- TA.fromArray axs :: Effect (ArrayView a)
      axs' <- TA.toArray xs'
      pure $ axs' === axs


allAreFilledTests :: Ref Int -> Effect Unit
allAreFilledTests count = overAll count allAreFilled
  where
    allAreFilled :: forall a b t. TestableArrayF a b D0 t Result
    allAreFilled (WithOffset _ xs) = do
      e <- TA.at xs 0
      let x = fromMaybe zero e
          l = TA.length xs
      TA.fill x 0 l xs
      let b = TA.all (_ == x) xs
      pure (b <?> "All aren't the filled value")


setSingletonIsEqTests :: Ref Int -> Effect Unit
setSingletonIsEqTests count = overAll count setSingletonIsEq
  where
    setSingletonIsEq :: forall a b t. TestableArrayF a b D1 t Result
    setSingletonIsEq (WithOffset os xs) = do
      e <- TA.at xs 0
      case e of
            Nothing -> pure Success
            Just x -> do
              let o = Vec.head os
              _ <- TA.set xs (Just o) [x]
              e' <- TA.at xs o
              pure $ e' === Just x


-- | Should work with any arbitrary predicate, but we can't generate them
allImpliesAnyTests :: Ref Int -> Effect Unit
allImpliesAnyTests count = overAll count allImpliesAny
  where
    allImpliesAny :: forall a b t. TestableArrayF a b D0 t Result
    allImpliesAny (WithOffset _ xs) =
      let pred x = x /= zero
          all' = TA.all pred xs <?> "All don't satisfy the predicate"
          any' = TA.any pred xs <?> "None satisfy the predicate"
      in pure $ (TA.length xs === zero) |=| all' ==> any'


-- | Should work with any arbitrary predicate, but we can't generate them
filterImpliesAllTests :: Ref Int -> Effect Unit
filterImpliesAllTests count = overAll count filterImpliesAll
  where
    filterImpliesAll :: forall a b t. TestableArrayF a b D0 t Result
    filterImpliesAll (WithOffset _ xs) =
      let pred x = x /= zero
          ys = TA.filter pred xs
          all' = TA.all pred ys
      in pure $ all' <?> "Filter doesn't imply all"


-- | Should work with any arbitrary predicate, but we can't generate them
filterIsTotalTests :: Ref Int -> Effect Unit
filterIsTotalTests count = overAll count filterIsTotal
  where
    filterIsTotal :: forall a b t. TestableArrayF a b D0 t Result
    filterIsTotal (WithOffset _ xs) = do
      let pred x = x /= zero
          ys = TA.filter pred xs
          zs = TA.filter (not pred) ys
      azs <- TA.toArray zs
      pure $ azs === []


-- | Should work with any arbitrary predicate, but we can't generate them
filterIsIdempotentTests :: Ref Int -> Effect Unit
filterIsIdempotentTests count = overAll count filterIsIdempotent
  where
    filterIsIdempotent :: forall a b t. TestableArrayF a b D0 t Result
    filterIsIdempotent (WithOffset _ xs) = do
      let pred x = x /= zero
          ys = TA.filter pred xs
          zs = TA.filter pred ys
      azs <- TA.toArray zs
      ays <- TA.toArray ys
      pure $ azs === ays


withOffsetHasIndexTests :: Ref Int -> Effect Unit
withOffsetHasIndexTests count = overAll1 count withOffsetHasIndex
  where
    withOffsetHasIndex :: forall a b t. TestableArrayF a b D5 t Result
    withOffsetHasIndex (WithOffset os xs) = pure $
      Array.all (TA.hasIndex xs) os <?> "All doesn't have index of itself"


withOffsetElemTests :: Ref Int -> Effect Unit
withOffsetElemTests count = overAll1 count withOffsetElem
  where
    withOffsetElem :: forall a b t. TestableArrayF a b D5 t Result
    withOffsetElem (WithOffset os xs) = pure $
      Array.all (\o -> TA.elem (unsafePartial $ unsafePerformEffect $ TA.unsafeAt xs o) Nothing xs) os
        <?> "All doesn't have an elem of itself"


-- | Should work with any arbitrary predicate, but we can't generate them
anyImpliesFindTests :: Ref Int -> Effect Unit
anyImpliesFindTests count = overAll count anyImpliesFind
  where
    anyImpliesFind :: forall a b t. TestableArrayF a b D0 t Result
    anyImpliesFind (WithOffset _ xs) = do
      let pred x = x /= zero
          p = TA.any pred xs <?> "All don't satisfy the predicate"
      idx <- TA.find pred xs
      let q = case idx of
              Nothing -> Failed "Doesn't have a value satisfying the predicate"
              Just z -> if pred z
                        then Success
                        else Failed "Found value doesn't satisfy the predicate"
      pure $ p ==> q


-- | Should work with any arbitrary predicate, but we can't generate them
findIndexImpliesAtTests :: Ref Int -> Effect Unit
findIndexImpliesAtTests count = overAll count findIndexImpliesAt
  where
    findIndexImpliesAt :: forall a b t. TestableArrayF a b D0 t Result
    findIndexImpliesAt (WithOffset _ xs) = do
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
indexOfImpliesAtTests count = overAll count indexOfImpliesAt
  where
    indexOfImpliesAt :: forall a b t. TestableArrayF a b D1 t Result
    indexOfImpliesAt (WithOffset _ xs) = do
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
lastIndexOfImpliesAtTests count = overAll count lastIndexOfImpliesAt
  where
    lastIndexOfImpliesAt :: forall a b t. TestableArrayF a b D0 t Result
    lastIndexOfImpliesAt (WithOffset _ xs) = do
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
foldrConsIsToArrayTests count = overAll count foldrConsIsToArray
  where
    foldrConsIsToArray :: forall a b t. TestableArrayF a b D0 t Result
    foldrConsIsToArray (WithOffset _ xs) = do
      axs <- TA.toArray xs
      pure $ TA.foldr Array.cons [] xs === axs


foldlSnocIsToArrayTests :: Ref Int -> Effect Unit
foldlSnocIsToArrayTests count = overAll count foldlSnocIsToArray
  where
    foldlSnocIsToArray :: forall a b t. TestableArrayF a b D0 t Result
    foldlSnocIsToArray (WithOffset _ xs) = do
      axs <- TA.toArray xs
      pure $ TA.foldl Array.snoc [] xs === axs


mapIdentityIsIdentityTests :: Ref Int -> Effect Unit
mapIdentityIsIdentityTests count = overAll count mapIdentityIsIdentity
  where
    mapIdentityIsIdentity :: forall a b t. TestableArrayF a b D0 t Result
    mapIdentityIsIdentity (WithOffset _ xs) = do
      axs <- TA.toArray xs
      mxs <- TA.toArray (TA.map identity xs)
      pure $ axs === mxs


traverseSnocIsToArrayTests :: Ref Int -> Effect Unit
traverseSnocIsToArrayTests count = overAll count traverseSnocIsToArray
  where
    traverseSnocIsToArray :: forall a b t. TestableArrayF a b D0 t Result
    traverseSnocIsToArray (WithOffset _ xs) = do
      ref <- Ref.new []
      TA.traverse_ (\x -> void (Ref.modify (\xs' -> Array.snoc xs' x) ref)) xs
      ys <- Ref.read ref
      axs <- TA.toArray xs
      pure $ axs === ys


doubleReverseIsIdentityTests :: Ref Int -> Effect Unit
doubleReverseIsIdentityTests count = overAll count doubleReverseIsIdentity
  where
    doubleReverseIsIdentity :: forall a b t. TestableArrayF a b D0 t Result
    doubleReverseIsIdentity (WithOffset _ xs) = do
      axs <- TA.toArray xs
      TA.reverse xs
      TA.reverse xs
      axs' <- TA.toArray xs
      pure $ axs === axs'


reverseIsArrayReverseTests :: Ref Int -> Effect Unit
reverseIsArrayReverseTests count = overAll count reverseIsArrayReverse
  where
    reverseIsArrayReverse :: forall a b t. TestableArrayF a b D0 t Result
    reverseIsArrayReverse (WithOffset _ xs) = do
      axs <- TA.toArray xs
      TA.reverse xs
      rxs <- TA.toArray xs
      pure $ Array.reverse axs === rxs


sortIsIdempotentTests :: Ref Int -> Effect Unit
sortIsIdempotentTests count = overAll count sortIsIdempotent
  where
    sortIsIdempotent :: forall a b t. TestableArrayF a b D0 t Result
    sortIsIdempotent (WithOffset _ xs) = do
      TA.sort xs
      ys <- TA.toArray xs
      TA.sort xs
      zs <- TA.toArray xs
      pure $ zs === ys


sortIsArraySortTests :: Ref Int -> Effect Unit
sortIsArraySortTests count = overAll count sortIsArraySort
  where
    sortIsArraySort :: forall a b t. TestableArrayF a b D0 t Result
    sortIsArraySort (WithOffset _ xs) = do
      axs <- TA.toArray xs
      let ys = Array.sort axs
      TA.sort xs
      sxs <- TA.toArray xs
      pure $ sxs === ys


toStringIsJoinWithCommaTests :: Ref Int -> Effect Unit
toStringIsJoinWithCommaTests count = overAll count toStringIsJoinWithComma
  where
    toStringIsJoinWithComma :: forall a b t. TestableArrayF a b D0 t Result
    toStringIsJoinWithComma (WithOffset _ xs) =
      pure $ TA.toString' xs "," === TA.toString xs


setTypedOfSubArrayIsIdentityTests :: Ref Int -> Effect Unit
setTypedOfSubArrayIsIdentityTests count = overAll count setTypedOfSubArrayIsIdentity
  where
    setTypedOfSubArrayIsIdentity :: forall a b t. TestableArrayF a b D0 t Result
    setTypedOfSubArrayIsIdentity (WithOffset _ xs) = do
      ys <- TA.toArray xs
      let l = TA.length xs
          zsSub = TA.subArray 0 l xs
      _ <- TA.setTyped xs Nothing zsSub
      zs <- TA.toArray xs
      pure $ zs === ys


modifyingOriginalMutatesSubArrayTests :: Ref Int -> Effect Unit
modifyingOriginalMutatesSubArrayTests count = overAll count modifyingOriginalMutatesSubArray
  where
    modifyingOriginalMutatesSubArray :: forall a b t. TestableArrayF a b D0 t Result
    modifyingOriginalMutatesSubArray (WithOffset _ xs) = do
      axs <- TA.toArray xs
      if Array.all (eq zero) axs
        then pure Success
        else do
        let l = TA.length xs
            zsSub = TA.subArray 0 l xs
        zs <- TA.toArray zsSub
        TA.fill zero 0 l xs
        ys <-  TA.toArray zsSub
        pure $ zs /== ys


modifyingSubArrayMutatesOriginalTests :: Ref Int -> Effect Unit
modifyingSubArrayMutatesOriginalTests count = overAll count modifyingOriginalMutatesSubArray
  where
    modifyingOriginalMutatesSubArray :: forall a b t. TestableArrayF a b D0 t Result
    modifyingOriginalMutatesSubArray (WithOffset _ xs) = do
      axs <- TA.toArray xs
      if Array.all (eq zero) axs
        then pure Success
        else do
        let l = TA.length xs
            zsSub = TA.subArray 0 l xs
        zs <- TA.toArray xs
        TA.fill zero 0 l zsSub
        ys <- TA.toArray xs
        pure $ zs /== ys


modifyingOriginalMutatesSubArrayZeroTests :: Ref Int -> Effect Unit
modifyingOriginalMutatesSubArrayZeroTests count = overAll count modifyingOriginalMutatesSubArrayZero
  where
    modifyingOriginalMutatesSubArrayZero :: forall a b t. TestableArrayF a b D0 t Result
    modifyingOriginalMutatesSubArrayZero (WithOffset _ xs) = do
      axs <- TA.toArray xs
      if Array.all (eq zero) axs
        then pure Success
        else do
        let l = TA.length xs
            zsSub = TA.subArray 0 l xs
        zs <- TA.toArray zsSub
        TA.fill zero 0 l xs
        ys <- TA.toArray zsSub
        pure $ zs /== ys


modifyingSubArrayMutatesOriginalZeroTests :: Ref Int -> Effect Unit
modifyingSubArrayMutatesOriginalZeroTests count = overAll count modifyingSubArrayMutatesOriginalZero
  where
    modifyingSubArrayMutatesOriginalZero :: forall a b t. TestableArrayF a b D0 t Result
    modifyingSubArrayMutatesOriginalZero (WithOffset _ xs) = do
      axs <- TA.toArray xs
      if Array.all (eq zero) axs
        then pure Success
        else do
        let l = TA.length xs
            zsSub = TA.subArray 0 l xs
        zs <- TA.toArray xs
        TA.fill zero 0 l zsSub
        ys <- TA.toArray xs
        pure $ zs /== ys


modifyingOriginalMutatesSubArrayAllTests :: Ref Int -> Effect Unit
modifyingOriginalMutatesSubArrayAllTests count = overAll count modifyingOriginalMutatesSubArrayAll
  where
    modifyingOriginalMutatesSubArrayAll :: forall a b t. TestableArrayF a b D0 t Result
    modifyingOriginalMutatesSubArrayAll (WithOffset _ xs) = do
      axs <- TA.toArray xs
      if Array.all (eq zero) axs
        then pure Success
        else do
        let l = TA.length xs
            zsSub = TA.subArray 0 l xs
        zs <- TA.toArray zsSub
        TA.fill zero 0 l xs
        ys <- TA.toArray zsSub
        pure $ zs /== ys


modifyingSubArrayMutatesOriginalAllTests :: Ref Int -> Effect Unit
modifyingSubArrayMutatesOriginalAllTests count = overAll count modifyingSubArrayMutatesOriginalAll
  where
    modifyingSubArrayMutatesOriginalAll :: forall a b t. TestableArrayF a b D0 t Result
    modifyingSubArrayMutatesOriginalAll (WithOffset _ xs) = do
      axs <- TA.toArray xs
      if Array.all (eq zero) axs
        then pure Success
        else do
        let l = TA.length xs
            zsSub = TA.subArray 0 l xs
        zs <- TA.toArray xs
        TA.fill zero 0 l zsSub
        ys <- TA.toArray xs
        pure $ zs /== ys


modifyingOriginalMutatesSubArrayPartTests :: Ref Int -> Effect Unit
modifyingOriginalMutatesSubArrayPartTests count = overAll count modifyingOriginalMutatesSubArrayPart
  where
    modifyingOriginalMutatesSubArrayPart :: forall a b t. TestableArrayF a b D1 t Result
    modifyingOriginalMutatesSubArrayPart (WithOffset os xs) = do
      let o = Vec.head os
          l = TA.length xs
          zsSub = TA.subArray 0 l xs
      zs <- TA.toArray zsSub
      if o == 0 || Array.all (eq zero) zs
        then pure Success
        else do
        TA.fill zero 0 l xs
        ys <- TA.toArray zsSub
        pure $ zs /== ys


modifyingOriginalDoesntMutateSliceTests :: Ref Int -> Effect Unit
modifyingOriginalDoesntMutateSliceTests count = overAll count modifyingOriginalDoesntMutateSlice
  where
    modifyingOriginalDoesntMutateSlice :: forall a b t. TestableArrayF a b D0 t Result
    modifyingOriginalDoesntMutateSlice (WithOffset _ xs) = do
      axs <- TA.toArray xs
      if Array.all (eq zero) axs
        then pure Success
        else do
        let l = TA.length xs
        zsSub <- TA.slice 0 l xs
        zs <- TA.toArray zsSub
        TA.fill zero 0 l xs
        ys <- TA.toArray zsSub
        pure $ zs === ys


modifyingOriginalDoesntMutateSlicePartTests :: Ref Int -> Effect Unit
modifyingOriginalDoesntMutateSlicePartTests count = overAll count modifyingOriginalDoesntMutateSlicePart
  where
    modifyingOriginalDoesntMutateSlicePart :: forall a b t. TestableArrayF a b D1 t Result
    modifyingOriginalDoesntMutateSlicePart (WithOffset os xs) = do
      let l = TA.length xs
      axs <- TA.toArray =<< TA.slice 0 l xs
      let o = Vec.head os
      e <- TA.at xs o
      if Array.all (eq zero) axs || e == Just zero
        then pure Success
        else do
        zsSub <- TA.slice o l xs
        zs <- TA.toArray zsSub
        TA.fill zero 0 l xs
        ys <- TA.toArray zsSub
        pure $ zs === ys


modifyingOriginalDoesntMutateSlicePart2Tests :: Ref Int -> Effect Unit
modifyingOriginalDoesntMutateSlicePart2Tests count = overAll count modifyingOriginalDoesntMutateSlicePart2
  where
    modifyingOriginalDoesntMutateSlicePart2 :: forall a b t. TestableArrayF a b D1 t Result
    modifyingOriginalDoesntMutateSlicePart2 (WithOffset os xs) = do
      let o = Vec.head os
          l = TA.length xs
      axs <- TA.toArray =<< TA.slice o l xs
      e <- TA.at xs o
      if Array.all (eq zero) axs || e == Just zero
        then pure Success
        else do
        zsSub <- TA.slice o l xs
        zs <- TA.toArray zsSub
        TA.fill zero 0 l xs
        ys <- TA.toArray zsSub
        pure $ zs === ys


copyWithinSelfIsIdentityTests :: Ref Int -> Effect Unit
copyWithinSelfIsIdentityTests count = overAll count copyWithinSelfIsIdentity
  where
    copyWithinSelfIsIdentity :: forall a b t. TestableArrayF a b D0 t Result
    copyWithinSelfIsIdentity (WithOffset _ xs) = do
      ys <- TA.toArray xs
      TA.copyWithin xs 0 0 (Just (TA.length xs))
      zs <- TA.toArray xs
      pure $ zs === ys


copyWithinIsSliceTests :: Ref Int -> Effect Unit
copyWithinIsSliceTests count = overAll count copyWithinIsSlice
  where
    copyWithinIsSlice :: forall a b t. TestableArrayF a b D1 t Result
    copyWithinIsSlice (WithOffset os xs) = do
      let o = Vec.head os
          l = TA.length xs
      ys <- TA.toArray =<< TA.slice o l xs
      TA.copyWithin xs 0 o Nothing
      axs <- TA.toArray xs
      zs <- pure $ Array.drop (Array.length ys) axs
      pure $ axs === ys <> zs


copyWithinViaSetTypedTests :: Ref Int -> Effect Unit
copyWithinViaSetTypedTests count = overAll count copyWithinViaSetTyped
  where
    copyWithinViaSetTyped :: forall a b t. TestableArrayF a b D1 t Result
    copyWithinViaSetTyped (WithOffset os xs) = do
      let o = Vec.head os
      txs <- TA.toArray xs
      xs' <- TA.fromArray txs :: Effect (ArrayView a)
      let l = TA.length xs'
      ys <- TA.slice o l xs'
      _ <- TA.setTyped xs' Nothing ys
      TA.copyWithin xs 0 o Nothing
      axs <- TA.toArray xs
      axs' <- TA.toArray xs'
      pure $ axs === axs'
