{ name = "arraybuffer"
, dependencies =
  [ "arraybuffer-types"
  , "arrays"
  , "console"
  , "effect"
  , "float32"
  , "foldable-traversable"
  , "functions"
  , "gen"
  , "maybe"
  , "nullable"
  , "partial"
  , "prelude"
  , "quickcheck"
  , "quickcheck-laws"
  , "refs"
  , "tailrec"
  , "typelevel-prelude"
  , "uint"
  , "unfoldable"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
