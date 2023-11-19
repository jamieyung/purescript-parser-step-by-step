{ name = "purescript-parser-step-by-step"
, dependencies =
  [ "aff"
  , "arrays"
  , "bifunctors"
  , "control"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "maybe"
  , "prelude"
  , "spec"
  , "strings"
  , "tailrec"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
