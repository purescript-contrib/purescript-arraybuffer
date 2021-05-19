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
  , "quickcheck-combinators"
  , "quickcheck-laws"
  , "refs"
  , "tailrec"
  , "typelevel"
  , "typelevel-prelude"
  , "uint"
  , "unfoldable"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
