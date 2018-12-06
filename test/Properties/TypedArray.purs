module Test.Properties.TypedArray where


import Data.ArrayBuffer.Types (ArrayView)
import Data.ArrayBuffer.Typed as TA
import Data.ArrayBuffer.Typed (class BytesPerValue)

import Prelude
import Data.Typelevel.Num (toInt', class Nat)
import Type.Proxy (Proxy (..))
import Test.QuickCheck (Result, (===))


byteLengthDivBytesPerValueEqLength :: forall a n. BytesPerValue a n => Nat n => ArrayView a -> Result
byteLengthDivBytesPerValueEqLength a =
  let n = toInt' (Proxy :: Proxy n)
  in  TA.length a === (TA.byteLength a `div` n)
