{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "affjax"
  , "argonaut"
  , "concur-react"
  , "console"
  , "data-default"
  , "effect"
  , "exists"
  , "halogen"
  , "nested-functor"
  , "numbers"
  , "psci-support"
  , "quickcheck"
  , "tuples"
  , "undefined"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
