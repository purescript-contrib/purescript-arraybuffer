module Test.Main where

import Prelude ( Unit, negate, bind, ($), (==), (&&), (<$>), pure
               , (=<<), (*), (-) )
import Data.Maybe (Maybe(..))
import Test.QuickCheck
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
            , reader :: DV.READER
            , writer :: DV.WRITER
            | e )
            Unit
main = do
  let ab = AB.create 4
  assert $ AB.byteLength ab == 4
  assert $ AB.byteLength (AB.slice 0 2 ab) == 2
  assert $ AB.byteLength (AB.slice 2 4 ab) == 2
  assert $ AB.byteLength (AB.slice (-2) (-2) ab) == 0
  assert $ AB.byteLength (AB.slice (-2) (-1) ab) == 1
  assert $ (DV.byteLength <$> (DV.slice 0 2 ab)) == Just 2
  assert $ (DV.byteLength <$> (DV.slice 2 2 ab)) == Just 2
  let aab = AB.fromArray [1.0, 2.0, 3.0, 4.0]
  assert $ AB.byteLength aab == 4
  let sab = AB.fromString "hola"
  assert $ AB.byteLength sab == 8

  let nab = AB.create 8
  let dv = DV.whole nab
  assert $ AB.byteLength (DV.buffer dv) == 8

  assert $ AB.byteLength (DV.buffer $ TA.dataView (TA.asInt8Array dv)) == 8

  assert $ (DV.byteLength <$> DV.slice 0 4 nab) == Just 4
  assert $ (DV.byteLength <$> DV.slice 0 40 nab) == Nothing

  assert $ do
     let ab' = AB.fromArray [1.0,2.0,3.0,4.0]
     let dv' = DV.whole ab'
     let i8 = TA.asInt8Array dv'
     ((Just 2.0) == i8 `TA.at` 1) && (Nothing == i8 `TA.at` 4) && (Nothing == i8 `TA.at` (-1))

  assert $ [1.0,2.0,3.0] == (TA.toArray $ TA.asInt8Array $ DV.whole $ AB.fromArray [1.0,2.0,3.0])

  assert =<< do
    let ab' = AB.create 2
    let dv' = DV.whole ab'
    DV.setUint8 dv' 123 0
    DV.setUint8 dv' 0 1
    leVal <- DV.getUint16le dv' 0
    beVal <- DV.getUint16be dv' 0
    pure ((Just 123 == leVal) && (Just 31488 == beVal))

  assert =<< do
    let ab' = AB.create 4
    let dv' = DV.whole ab'
    DV.setUint8 dv' 255 0
    DV.setUint8 dv' 255 1
    DV.setUint8 dv' 255 2
    DV.setUint8 dv' 255 3
    val <- DV.getUint32be dv' 0
    let expected = fromInt 2 `pow` fromInt 32 - fromInt 1
    pure (Just expected == val)

assert :: forall e. Boolean -> QC e Unit
assert = quickCheck' 1
