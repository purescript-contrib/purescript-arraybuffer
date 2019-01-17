module Test.Properties where

import Test.Properties.TypedArray (typedArrayTests)
import Test.Properties.DataView (dataViewTests)

import Prelude (Unit, bind, discard, ($), (<>), (*), show)
import Effect (Effect)
import Effect.Ref (new, read) as Ref
import Effect.Console (log)


propertiesTests :: Effect Unit
propertiesTests = do
  do
    count <- Ref.new 0
    log "  - TypedArray Tests:"
    typedArrayTests count
    c <- Ref.read count
    log $ "  - Verified " <> show c <> " properties, generating " <> show (c * 9 * 100) <> " test cases."

  do
    count <- Ref.new 0
    log "  - DataView Tests:"
    dataViewTests count
    c <- Ref.read count
    log $ "  - Verified " <> show c <> " properties, generating " <> show (c * 16 * 100) <> " test cases."
