
let upstream =
     https://github.com/purescript/package-sets/releases/download/psc-0.14.1-20210419/packages.dhall sha256:d9a082ffb5c0fabf689574f0680e901ca6f924e01acdbece5eeedd951731375a

let overrides = {=}

let additions =
  { float32 =
    { dependencies =
      [ "effect"
      , "gen"
      , "maybe"
      , "prelude"
      ]
    , repo =
        "https://github.com/athanclark/purescript-float32.git"
    , version =
        "v0.2.0"
    }
  , uint =
    { dependencies =
      [ "effect"
      , "math"
      , "maybe"
      , "quickcheck"
      , "quickcheck-laws"
      ]
    , repo = "https://github.com/purescript-contrib/purescript-uint.git"
    , version = "v5.1.4"
    }
  }

in  upstream // overrides // additions

