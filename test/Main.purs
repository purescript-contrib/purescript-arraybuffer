module Test.Main where

import Test.Properties (propertiesTests)

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref as Ref




main :: Effect Unit
main = do

  log "Starting tests..."
  propertiesTests



-- import Data.ArrayBuffer.ArrayBuffer as AB
-- import Data.ArrayBuffer.DataView as DV
-- import Data.ArrayBuffer.Typed as TA
-- import Data.Either (fromRight)
-- import Data.Maybe (Maybe(..), isNothing)
-- import Data.UInt (fromInt, pow)
-- import Partial.Unsafe (unsafePartial)
-- import Test.QuickCheck (quickCheck', (<?>), quickCheck)
-- import Test.Input (WellFormedInput(..))


-- assertEffEquals :: forall a. Eq a => Show a => a -> Effect a -> Effect Unit
-- assertEffEquals expectedValue computation = do
--   actualValue <- computation
--   let msg = show expectedValue <> " /= " <> show actualValue
--   quickCheck' 1 $ actualValue == expectedValue <?> msg

-- assertEquals :: forall a. Eq a => Show a => a -> a -> Effect Unit
-- assertEquals expected actual = do
--   let msg = show expected <> " /= " <> show actual
--   quickCheck' 1 $ expected == actual <?> msg

-- main :: Effect Unit
-- main = do
--   assertEquals "釺椱�밸造ə㊡癥闗" (unsafePartial $ fromRight $ AB.decodeToString $ AB.fromString "釺椱�밸造ə㊡癥闗")

--   quickCheck
--     \(WellFormedInput s) ->
--         let
--           result = (unsafePartial $ fromRight $ AB.decodeToString $ AB.fromString s)
--         in
--           s == result
--           <?> "Isormorphic arraybuffer conversion with string failed for input\n"
--           <> s
--           <> " which, after the round trip, result in\n"
--           <> result

--   ab4 <- AB.create 4
--   ab8 <- AB.create 8
--   assertEquals 4 $ AB.byteLength ab4
--   assertEffEquals 2 $ pure <<< AB.byteLength =<< AB.slice 0 2 ab4
--   assertEffEquals 2 $ pure <<< AB.byteLength =<< AB.slice 2 4 ab4
--   assertEffEquals 0 $ pure <<< AB.byteLength =<< AB.slice (-2) (-2) ab4
--   assertEffEquals 1 $ pure <<< AB.byteLength =<< (AB.slice (-2) (-1) ab4)
--   assertEquals Nothing $ DV.byteLength <$> DV.slice 0 200 ab4
--   assertEquals (Just 2) $ DV.byteLength <$> DV.slice 0 2 ab4
--   assertEquals (Just 2) $ DV.byteLength <$> DV.slice 2 2 ab4
--   assertEquals 4 $ AB.byteLength $ AB.fromArray [1.0, 2.0, 3.0, 4.0]
--   assertEquals 4 $ AB.byteLength $ AB.fromIntArray [1, 2, 3, 4]
--   assertEquals 4 $ AB.byteLength $ AB.fromString "hola"
--   assertEquals 5 $ AB.byteLength $ AB.fromString "hóla"
--   assertEquals 7 $ AB.byteLength $ AB.fromString "hóla¡"
--   assertEquals 8 $ AB.byteLength $ DV.buffer $ DV.whole ab8
--   assertEquals 8 $ AB.byteLength $ DV.buffer $ TA.dataView $ TA.asInt8Array $ DV.whole ab8

--   assertEquals (Just 8) $ DV.byteLength <$> DV.slice 0 8 ab8
--   assertEquals true $ isNothing $ DV.slice 0 40 ab8

--   fourElementInt8Array <- pure <<< TA.asInt8Array <<< DV.whole $ AB.fromIntArray [1, 2, 3, 4]
--   assertEffEquals (Just 2.0) $ TA.at fourElementInt8Array 1
--   assertEffEquals Nothing $ TA.at fourElementInt8Array 4
--   assertEffEquals Nothing $ TA.at fourElementInt8Array (-1)

--   assertEquals [1.0, 2.0, 3.0] $ TA.toArray <<< TA.asInt8Array <<< DV.whole $ AB.fromArray [1.0, 2.0, 3.0]

--   twoElementDataView <- do
--     ab' <- AB.create 2
--     let dv = DV.whole ab'
--     DV.setUint8 dv (fromInt 123) 0
--     DV.setUint8 dv (fromInt 0) 1
--     pure dv
--   assertEffEquals (Just $ fromInt 123) $ DV.getUint16le twoElementDataView 0
--   assertEffEquals (Just $ fromInt 31488) $ DV.getUint16be twoElementDataView 0
--   assertEffEquals (Just $ fromInt 2 `pow` fromInt 32 - fromInt 1) $ do
--     ab' <- AB.create 4
--     let dv = DV.whole ab'
--         t = fromInt 255
--     DV.setUint8 dv t 0
--     DV.setUint8 dv t 1
--     DV.setUint8 dv t 2
--     DV.setUint8 dv t 3
--     DV.getUint32be dv 0

--   let arr = DV.whole (AB.fromIntArray [0x4, 0x3, 0x2, 0x1])

--   assertEffEquals (Just 0x04) (DV.getInt8 arr 0)
--   assertEffEquals (Just 0x04) (DV.getInt8 (TA.dataView (TA.asInt8Array arr)) 0)
--   assertEffEquals (Just 0x0304) (DV.getInt16le arr 0)
--   assertEffEquals (Just 0x0304) (DV.getInt16le (TA.dataView (TA.asInt16Array arr)) 0)
--   assertEffEquals (Just 0x01020304) (DV.getInt32le arr 0)
--   assertEffEquals (Just 0x01020304) (DV.getInt32le (TA.dataView (TA.asInt32Array arr)) 0)
