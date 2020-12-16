{ name = "lingolive"
, dependencies =
  [ "console"
  , "effect"
  , "psci-support"
  , "nullable"
  , "functions"
  , "protolude"
  , "affjax"
  , "spec"
  , "generics-rep"
  , "record"
  , "strings"
  , "node-path"
  , "node-fs-aff"
  , "ansi"
  , "argonaut-codecs"
  , "argonaut-generic"
  , "generics-rep"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}