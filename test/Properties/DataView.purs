module Test.Properties.DataView where


import Data.ArrayBuffer.Types (DataView)
import Data.ArrayBuffer.DataView as DA
import Data.ArrayBuffer.DataView.Gen (genDataView)

import Prelude
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref



dataViewTests :: Ref Int -> Effect Unit
dataViewTests count = do
  pure unit


type TestableViewF q =
  DataView
  -> q


overAll :: Ref Int -> Effect Unit
overAll count = do
  void (Ref.modify (\x -> x + 1) count)
