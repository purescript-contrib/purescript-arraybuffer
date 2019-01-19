module Test.Properties where

import Effect (Effect)
import Effect.Console (log)
import Effect.Ref (new, read) as Ref
import Prelude (Unit, bind, discard, ($), (<>), (*), show)
import Test.Properties.DataView (dataViewTests)
import Test.Properties.TypedArray (typedArrayTests)


propertiesTests :: Effect Unit
propertiesTests = do
  do
    count <- Ref.new 0
    log "  - TypedArray Tests:"
    typedArrayTests count
    c <- Ref.read count
    log $ "  - Verified " <> show c <> " properties, generating " <> show (c * 9 * 100) <> " test cases."

  -- do
  --   count <- Ref.new 0
  --   log "  - DataView Tests:"
  --   dataViewTests count
  --   c <- Ref.read count
  --   log $ "  - Verified " <> show c <> " properties, generating " <> show (c * 16 * 100) <> " test cases."
