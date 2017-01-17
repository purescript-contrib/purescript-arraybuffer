module Test.Main where

import Prelude
import Data.ArrayBuffer.ArrayBuffer as AB
import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.Typed as TA
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Data.Maybe (Maybe(..), fromJust)
import Data.UInt (fromInt, pow)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (QC, quickCheck')

assertEffEquals :: forall a e. Eq a => QC e a -> a -> QC e Unit
assertEffEquals computation expectedValue = do
  actualValue <- computation
  quickCheck' 1 $ actualValue == expectedValue

main :: forall e
      . Eff ( console :: CONSOLE
            , random :: RANDOM
            , err :: EXCEPTION
            , arrayBuffer :: AB.ARRAYBUFFER
            | e )
            Unit
main = do
  ab <- AB.create 4
  assertEffEquals (AB.byteLength ab) 4
  assertEffEquals (AB.byteLength =<< (AB.slice 0 2 ab)) 2
  assertEffEquals (AB.byteLength =<< (AB.slice 2 4 ab)) 2
  assertEffEquals (AB.byteLength =<< (AB.slice (-2) (-2) ab)) 0
  assertEffEquals (AB.byteLength =<< (AB.slice (-2) (-1) ab)) 1
  maybeSliceFromStart <- DV.slice 0 2 ab
  let sliceFromStart = unsafePartial $ fromJust maybeSliceFromStart
  assertEffEquals (DV.byteLength sliceFromStart) 2
  maybeSliceFromSecond <- DV.slice 2 2 ab
  let sliceFromSecond = unsafePartial $ fromJust maybeSliceFromSecond
  assertEffEquals (DV.byteLength sliceFromSecond) 2
  aab <- AB.fromArray [1.0, 2.0, 3.0, 4.0]
  assertEffEquals (AB.byteLength aab) 4
  sab <- AB.fromString "hola"
  assertEffEquals (AB.byteLength sab) 8

  nab <- AB.create 8
  dv <- DV.whole nab
  nab' <- DV.buffer dv
  assertEffEquals (AB.byteLength nab') 8

  assertEffEquals (AB.byteLength =<< DV.buffer =<< TA.dataView (TA.asInt8Array dv)) 8

  maybeNabSlice <- (DV.slice 0 4 nab)
  let nabSlice = unsafePartial $ fromJust maybeNabSlice
  assertEffEquals (DV.byteLength nabSlice) 4
  -- assertEffEquals (DV.slice 0 40 nab) Nothing

  fourElementArrayBuffer <- AB.fromArray [1.0, 2.0, 3.0, 4.0]
  fourElementDataView <- DV.whole fourElementArrayBuffer
  let i8 = TA.asInt8Array fourElementDataView
  assertEffEquals (TA.at i8 1) (Just 2.0)
  assertEffEquals (TA.at i8 4) Nothing
  assertEffEquals (TA.at i8 (-1)) Nothing

  threeDataView <- AB.fromArray [1.0, 2.0, 3.0] >>= DV.whole
  let threeInt8Array = TA.asInt8Array threeDataView
  assertEffEquals (TA.toArray threeInt8Array) [1.0, 2.0, 3.0]

  twoElementArrayBuffer <- AB.create 2
  twoElementDataView <- DV.whole twoElementArrayBuffer
  DV.setUint8 twoElementDataView 123 0
  DV.setUint8 twoElementDataView 0 1
  assertEffEquals (DV.getUint16le twoElementDataView 0) (Just 123)
  assertEffEquals (DV.getUint16be twoElementDataView 0) (Just 31488)

  fourElementArrayBuffer' <- AB.create 4
  fourElementDataView' <- DV.whole fourElementArrayBuffer'
  DV.setUint8 fourElementDataView' 255 0
  DV.setUint8 fourElementDataView' 255 1
  DV.setUint8 fourElementDataView' 255 2
  DV.setUint8 fourElementDataView' 255 3
  assertEffEquals (DV.getUint32be fourElementDataView' 0) (Just $ fromInt 2 `pow` fromInt 32 - fromInt 1)
