{ name = "arraybuffer"
, dependencies =
  [ "arraybuffer-types"
  , "arrays"
  , "effect"
  , "float32"
  , "functions"
  , "gen"
  , "maybe"
  , "nullable"
  , "prelude"
  , "tailrec"
  , "uint"
  , "unfoldable"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "MIT"
, repository = "https://github.com/purescript-contrib/purescript-arraybuffer"
}
