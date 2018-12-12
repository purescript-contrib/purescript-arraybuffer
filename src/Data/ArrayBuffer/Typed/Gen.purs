-- | Functions for generating typed arrays and values.

module Data.ArrayBuffer.Typed.Gen where

import Data.ArrayBuffer.Types
  ( Uint8ClampedArray, Uint8Array, Uint16Array, Uint32Array
  , Int8Array, Int16Array, Int32Array
  , Float32Array, Float64Array, ArrayView
  )
import Data.ArrayBuffer.Typed as TA

import Prelude
import Math as M
import Data.Maybe (Maybe (..))
import Data.List.Lazy (replicateM)
import Data.Int as I
import Data.UInt (UInt)
import Data.UInt as UInt
import Data.String.CodeUnits as S
import Data.Float.Parse (parseFloat)
import Data.Array as Array
import Data.Vec (Vec)
import Data.Vec (fromArray) as Vec
import Data.Generic.Rep (class Generic)
import Data.Typelevel.Num (class Nat, toInt')
import Data.Unfoldable (replicateA)
import Type.Proxy (Proxy (..))
import Control.Monad.Gen.Class (class MonadGen, sized, chooseInt, chooseFloat)
import Partial.Unsafe (unsafePartial)




genUint8ClampedArray :: forall m. MonadGen m => m Uint8ClampedArray
genUint8ClampedArray = sized \s ->
   TA.fromArray <<< Array.fromFoldable <$> replicateM s genUByte

genUint32Array :: forall m. MonadGen m => m Uint32Array
genUint32Array = sized \s ->
   TA.fromArray <<< Array.fromFoldable <$> replicateM s genUWord

genUint16Array :: forall m. MonadGen m => m Uint16Array
genUint16Array = sized \s ->
   TA.fromArray <<< Array.fromFoldable <$> replicateM s genUChomp

genUint8Array :: forall m. MonadGen m => m Uint8Array
genUint8Array = sized \s ->
   TA.fromArray <<< Array.fromFoldable <$> replicateM s genUByte

genInt32Array :: forall m. MonadGen m => m Int32Array
genInt32Array = sized \s ->
   TA.fromArray <<< Array.fromFoldable <$> replicateM s genWord

genInt16Array :: forall m. MonadGen m => m Int16Array
genInt16Array = sized \s ->
   TA.fromArray <<< Array.fromFoldable <$> replicateM s genChomp

genInt8Array :: forall m. MonadGen m => m Int8Array
genInt8Array = sized \s ->
   TA.fromArray <<< Array.fromFoldable <$> replicateM s genByte

genFloat32Array :: forall m. MonadGen m => m Float32Array
genFloat32Array = sized \s ->
   TA.fromArray <<< Array.fromFoldable <$> replicateM s genFloat32

genFloat64Array :: forall m. MonadGen m => m Float64Array
genFloat64Array = sized \s ->
   TA.fromArray <<< Array.fromFoldable <$> replicateM s genFloat64




genUByte :: forall m. MonadGen m => m Int
genUByte = chooseInt 0 ((I.pow 2 8) - 1)

genByte :: forall m. MonadGen m => m Int
genByte =
  let j = I.pow 2 4
  in  chooseInt (negate j) (j - 1)

genUChomp :: forall m. MonadGen m => m Int
genUChomp = chooseInt 0 ((I.pow 2 16) - 1)

genChomp :: forall m. MonadGen m => m Int
genChomp =
  let j = I.pow 2 8
  in  chooseInt (negate j) (j - 1)

genUWord :: forall m. MonadGen m => m UInt
genUWord = UInt.fromNumber <$> chooseFloat 0.0 ((M.pow 2.0 32.0) - 1.0)

genWord :: forall m. MonadGen m => m Int
genWord =
  let j = I.pow 2 16
  in  chooseInt (negate j) (j - 1)

genFloat32 :: forall m. MonadGen m => m Number
genFloat32 =
  let maxFloat32 = (1.0 - (M.pow 2.0 (-24.0))) * (M.pow 2.0 128.0)
      minFloat32 = -maxFloat32 -- because of sign bit
      reformat :: String -> String
      reformat s =
        let pre = S.takeWhile (\c -> c /= '.') s
            suf = S.dropWhile (\c -> c /= '.') s
        in  pre <> "." <> S.take 6 suf
      fix :: Number -> Number
      fix x = unsafePartial $ case parseFloat (reformat (show x)) of
        Just y -> y
  in  fix <$> chooseFloat minFloat32 maxFloat32
  -- roughly estimated because of variable precision between 6 and 9 digs

genFloat64 :: forall m. MonadGen m => m Number
genFloat64 = chooseFloat (-1.7e308) 1.7e308



-- | For generating some set of offsets residing inside the generated array
data WithOffset n a = WithOffset (Vec n TA.Offset) (ArrayView a)
derive instance genericWithOffset :: Generic (ArrayView a) a' => Generic (WithOffset n a) _

genWithOffset :: forall m n b a
               . MonadGen m
              => Nat n
              => TA.BytesPerValue a b
              => m (ArrayView a)
              -> m (WithOffset n a)
genWithOffset genArrayView = do
  let n = toInt' (Proxy :: Proxy n)
  xs <- genArrayView
  let l = TA.length xs
  mos <- replicateA n (chooseInt 0 (l - 1))
  let os = unsafePartial $ case Vec.fromArray mos of
        Just q -> q
  pure (WithOffset os xs)
