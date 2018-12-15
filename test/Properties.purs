module Test.Properties where

import Test.Properties.TypedArray (typedArrayTests)
import Test.Properties.DataView (dataViewTests)

import Prelude (Unit, bind, discard)
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Console (log)


propertiesTests :: Ref Int -> Effect Unit
propertiesTests count = do
  log "  - TypedArray Tests:"
  typedArrayTests count
  log "  - DataView Tests:"
  dataViewTests count
