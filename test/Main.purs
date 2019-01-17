module Test.Main where

import Test.Properties (propertiesTests)

import Prelude
import Effect (Effect)
import Effect.Console (log)




main :: Effect Unit
main = do

  log "Starting tests..."
  propertiesTests
