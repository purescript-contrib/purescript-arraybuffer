let conf = ./spago.dhall
in conf // {
, dependencies = conf.dependencies #
  [ "console"
  , "foldable-traversable"
  , "partial"
  , "refs"
  , "tuples"
  , "quickcheck"
  , "quickcheck-laws"
  ]
, sources = conf.sources # [ "test/**/*.purs" ]
}
