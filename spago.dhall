{ name = "setup-purescript"
, dependencies =
  [ "aff"
  , "aff-retry"
  , "affjax"
  , "argonaut-codecs"
  , "argonaut-core"
  , "arrays"
  , "bifunctors"
  , "effect"
  , "either"
  , "enums"
  , "exceptions"
  , "foldable-traversable"
  , "github-actions-toolkit"
  , "integers"
  , "math"
  , "maybe"
  , "node-buffer"
  , "node-fs"
  , "node-path"
  , "node-process"
  , "parsing"
  , "partial"
  , "prelude"
  , "refs"
  , "strings"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "versions"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
