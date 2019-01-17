-- | Functions for generating typed arrays and values.

module Data.ArrayBuffer.Typed.Gen where

import Data.ArrayBuffer.Types (ArrayView)
import Data.ArrayBuffer.Typed as TA
import Data.ArrayBuffer.ValueMapping (class BytesPerValue)

import Prelude
import Math as M
import Data.Maybe (Maybe (..))
import Data.Int as I
import Data.UInt (UInt)
import Data.UInt (fromInt, fromNumber) as UInt
import Data.String.CodeUnits as S
import Data.Float.Parse (parseFloat)
import Data.Vec (Vec)
import Data.Vec (fromArray) as Vec
import Data.Generic.Rep (class Generic)
import Data.Typelevel.Num (class Nat, toInt')
import Data.Unfoldable (replicateA)
import Type.Proxy (Proxy (..))
import Control.Monad.Gen.Class (class MonadGen, sized, chooseInt, chooseFloat)
import Partial.Unsafe (unsafePartial)




genTypedArray :: forall m a t
               . MonadGen m
              => TA.TypedArray a t
              => TA.Length -- ^ Min length
              -> Maybe TA.Length -- ^ Max length
              -> m t
              -> m (ArrayView a)
genTypedArray q1 mq2 gen = sized \s ->
  let s'' = s `max` q1
      s' = case mq2 of
        Nothing -> s''
        Just q2 -> s'' `min` q2
  in  TA.fromArray <$> replicateA s' gen



genUByte :: forall m. MonadGen m => m UInt
genUByte = UInt.fromInt <$> chooseInt 0 ((I.pow 2 8) - 1)

genByte :: forall m. MonadGen m => m Int
genByte =
  let j = I.pow 2 4
  in  chooseInt (negate j) (j - 1)

genUChomp :: forall m. MonadGen m => m UInt
genUChomp = UInt.fromInt <$> chooseInt 0 ((I.pow 2 16) - 1)

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
              => BytesPerValue a b
              => m (ArrayView a)
              -> m (WithOffset n a)
genWithOffset gen = do
  let n = toInt' (Proxy :: Proxy n)
  xs <- gen
  let l = TA.length xs
  mos <- replicateA n (chooseInt 0 (l - 1))
  let os = unsafePartial $ case Vec.fromArray mos of
        Just q -> q
  pure (WithOffset os xs)
