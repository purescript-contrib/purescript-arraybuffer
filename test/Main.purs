module Test.Main where

import Prelude

import Effect (Effect)
import Data.ArrayBuffer.ArrayBuffer as AB
import Data.ArrayBuffer.DataView as DV
import Data.ArrayBuffer.Typed as TA
import Data.ArrayBuffer.Types as AT
import Data.Maybe (Maybe(..), isNothing)
import Data.UInt (fromInt, pow)
import Test.QuickCheck (quickCheck', (<?>), quickCheck)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)

newtype ABuffer = ABuffer AT.ArrayBuffer

instance arbitraryArrayBuffer :: Arbitrary ABuffer where
  arbitrary = map (ABuffer <<< AB.fromString) arbitrary

assertEffEquals :: forall a. Eq a => Show a => a -> Effect a -> Effect Unit
assertEffEquals expectedValue computation = do
  actualValue <- computation
  let msg = show expectedValue <> " /= " <> show actualValue
  quickCheck' 1 $ actualValue == expectedValue <?> msg

assertEquals :: forall a. Eq a => Show a => a -> a -> Effect Unit
assertEquals expected actual = do
  let msg = show expected <> " /= " <> show actual
  quickCheck' 1 $ expected == actual <?> msg

main :: Effect Unit
main = do
  ab4 <- AB.create 4
  ab8 <- AB.create 8
  assertEquals 4 $ AB.byteLength ab4
  assertEffEquals 2 $ pure <<< AB.byteLength =<< AB.slice 0 2 ab4
  assertEffEquals 2 $ pure <<< AB.byteLength =<< AB.slice 2 4 ab4
  assertEffEquals 0 $ pure <<< AB.byteLength =<< AB.slice (-2) (-2) ab4
  assertEffEquals 1 $ pure <<< AB.byteLength =<< (AB.slice (-2) (-1) ab4)
  assertEquals Nothing $ DV.byteLength <$> DV.slice 0 200 ab4
  assertEquals (Just 2) $ DV.byteLength <$> DV.slice 0 2 ab4
  assertEquals (Just 2) $ DV.byteLength <$> DV.slice 2 2 ab4
  assertEquals 4 $ AB.byteLength $ AB.fromArray [1.0, 2.0, 3.0, 4.0]
  assertEquals 4 $ AB.byteLength $ AB.fromIntArray [1, 2, 3, 4]
  assertEquals 8 $ AB.byteLength $ AB.fromString "hola"
  assertEquals 8 $ AB.byteLength $ AB.fromString "hóla"
  assertEquals 10 $ AB.byteLength $ AB.fromString "hóla¡"
  assertEquals 8 $ AB.byteLength $ DV.buffer $ DV.whole ab8
  assertEquals 8 $ AB.byteLength $ DV.buffer $ TA.dataView $ TA.asInt8Array $ DV.whole ab8

  assertEquals (Just 8) $ DV.byteLength <$> DV.slice 0 8 ab8
  assertEquals true $ isNothing $ DV.slice 0 40 ab8

  fourElementInt8Array <- pure <<< TA.asInt8Array <<< DV.whole $ AB.fromIntArray [1, 2, 3, 4]
  assertEffEquals (Just 2.0) $ TA.at fourElementInt8Array 1
  assertEffEquals Nothing $ TA.at fourElementInt8Array 4
  assertEffEquals Nothing $ TA.at fourElementInt8Array (-1)

  quickCheck
    \(s) ->
      s == (AB.decodeToString $ AB.fromString s)
      <?> "Isormorphic arraybuffer conversion with string failed for input "
      <> s

  assertEquals [1.0, 2.0, 3.0] $ TA.toArray <<< TA.asInt8Array <<< DV.whole $ AB.fromArray [1.0, 2.0, 3.0]

  twoElementDataView <- do
    ab' <- AB.create 2
    let dv = DV.whole ab'
    DV.setUint8 dv (fromInt 123) 0
    DV.setUint8 dv (fromInt 0) 1
    pure dv
  assertEffEquals (Just $ fromInt 123) $ DV.getUint16le twoElementDataView 0
  assertEffEquals (Just $ fromInt 31488) $ DV.getUint16be twoElementDataView 0
  assertEffEquals (Just $ fromInt 2 `pow` fromInt 32 - fromInt 1) $ do
    ab' <- AB.create 4
    let dv = DV.whole ab'
        t = fromInt 255
    DV.setUint8 dv t 0
    DV.setUint8 dv t 1
    DV.setUint8 dv t 2
    DV.setUint8 dv t 3
    DV.getUint32be dv 0

