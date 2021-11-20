module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Test.Properties (propertiesTests)

main :: Effect Unit
main = do
  log "Starting tests..."
  propertiesTests
