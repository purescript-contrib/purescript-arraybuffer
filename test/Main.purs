module Test.Main where

import Prelude
import Data.Maybe (Maybe(..))
import Test.QuickCheck (QC, quickCheck')
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.UInt (fromInt, pow)
import Data.ArrayBuffer.ArrayBuffer as AB
import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.Typed as TA

main :: forall e
      . Eff ( console :: CONSOLE
            , random :: RANDOM
            , err :: EXCEPTION
            , arraybuffer :: AB.ARRAYBUFFER
            | e )
            Unit
main = do
  ab4 <- AB.create 4
  assert $ AB.byteLength ab4 == 4
  assert =<< do
    abs02 <- AB.slice 0 2 ab4
    pure $ AB.byteLength abs02 == 2
  assert =<< do
    abs24 <- AB.slice 2 4 ab4
    pure $ AB.byteLength abs24 == 2
  assert =<< do
    absm2m2 <- AB.slice (-2) (-2) ab4
    pure $ AB.byteLength absm2m2 == 0
  assert =<< do
    absm2m1 <- AB.slice (-2) (-1) ab4
    pure $ AB.byteLength absm2m1 == 1
  assert =<< do
    s02 <- DV.slice 0 2 ab4
    pure $ (DV.byteLength <$> s02) == Just 2
  assert =<< do
    s22 <- DV.slice 2 2 ab4
    pure $ (DV.byteLength <$> s22) == Just 2
  aab <- AB.fromArray [1.0, 2.0, 3.0, 4.0]
  assert $ AB.byteLength aab == 4
  sab <- AB.fromString "hola"
  assert $ AB.byteLength sab == 8

  nab <- AB.create 8
  dv <- DV.whole nab
  assert $ AB.byteLength (DV.buffer dv) == 8

  assert =<< do
    dvi8 <- TA.asInt8Array dv
    pure $ AB.byteLength (DV.buffer $ TA.dataView dvi8) == 8

  assert =<< do
    s04 <- DV.slice 0 4 nab
    pure $ (DV.byteLength <$> s04) == Just 4
  assert =<< do
    s040 <- DV.slice 0 40 nab
    pure $ (DV.byteLength <$> s040) == Nothing

  assert =<< do
    ab <- AB.fromArray [1.0,2.0,3.0,4.0]
    dv <- DV.whole ab
    i8 <- TA.asInt8Array dv
    a1 <- i8 `TA.at` 1
    a4 <- i8 `TA.at` 4
    am1 <- i8 `TA.at` (-1)
    pure $ ((Just 2.0) == a1) && (Nothing == a4) && (Nothing == am1)

  assert =<< do
    ab3 <- AB.fromArray [1.0,2.0,3.0]
    wab3 <- DV.whole ab3
    wab3i8 <- TA.asInt8Array wab3
    a3 <- TA.toArray wab3i8
    pure $ a3 == [1.0,2.0,3.0]

  ab2 <- AB.create 2
  assert =<< do
    dv <- DV.whole ab2
    DV.setUint8 dv 123 0
    DV.setUint8 dv 0 1
    leVal <- DV.getUint16le dv 0
    beVal <- DV.getUint16be dv 0
    pure $ Just 123 == leVal && Just 31488 == beVal

  assert =<< do
    ab <- AB.create 4
    dv <- DV.whole ab
    DV.setUint8 dv 255 0
    DV.setUint8 dv 255 1
    DV.setUint8 dv 255 2
    DV.setUint8 dv 255 3
    val <- DV.getUint32be dv 0
    let expected = fromInt 2 `pow` fromInt 32 - fromInt 1
    pure $ Just expected == val

assert :: forall e. Boolean -> QC e Unit
assert = quickCheck' 1

