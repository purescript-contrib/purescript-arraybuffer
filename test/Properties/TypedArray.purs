module Test.Properties.TypedArray where


import Data.ArrayBuffer.Types (ArrayView)
import Data.ArrayBuffer.Typed as TA
import Data.ArrayBuffer.Typed (class BytesPerValue)
import Data.ArrayBuffer.Typed.Gen
  ( arbitraryUint8ClampedArray, arbitraryUint8Array, arbitraryUint16Array, arbitraryUint32Array
  , arbitraryInt8Array, arbitraryInt16Array, arbitraryInt32Array
  , arbitraryFloat32Array, arbitraryFloat64Array)

import Prelude
import Data.Typelevel.Num (toInt', class Nat)
import Type.Proxy (Proxy (..))
import Test.QuickCheck (quickCheckGen, Result, (===))
import Effect (Effect)
import Effect.Console (log)



byteLengthDivBytesPerValueTests :: Effect Unit
byteLengthDivBytesPerValueTests = do
  log "  - byteLength x / bytesPerValue === length x"
  log "    - Uint8ClampedArray"
  quickCheckGen (byteLengthDivBytesPerValueEqLength <$> arbitraryUint8ClampedArray)
  log "    - Uint32Array"
  quickCheckGen (byteLengthDivBytesPerValueEqLength <$> arbitraryUint32Array)
  log "    - Uint16Array"
  quickCheckGen (byteLengthDivBytesPerValueEqLength <$> arbitraryUint16Array)
  log "    - Uint8Array"
  quickCheckGen (byteLengthDivBytesPerValueEqLength <$> arbitraryUint8Array)
  log "    - Int32Array"
  quickCheckGen (byteLengthDivBytesPerValueEqLength <$> arbitraryInt32Array)
  log "    - Int16Array"
  quickCheckGen (byteLengthDivBytesPerValueEqLength <$> arbitraryInt16Array)
  log "    - Int8Array"
  quickCheckGen (byteLengthDivBytesPerValueEqLength <$> arbitraryInt8Array)
  log "    - Float32Array"
  quickCheckGen (byteLengthDivBytesPerValueEqLength <$> arbitraryFloat32Array)
  log "    - Float64Array"
  quickCheckGen (byteLengthDivBytesPerValueEqLength <$> arbitraryFloat64Array)


byteLengthDivBytesPerValueEqLength :: forall a n. BytesPerValue a n => Nat n => ArrayView a -> Result
byteLengthDivBytesPerValueEqLength a =
  let n = toInt' (Proxy :: Proxy n)
  in  TA.length a === (TA.byteLength a `div` n)
