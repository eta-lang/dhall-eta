let prelude =
      https://raw.githubusercontent.com/eta-lang/dhall-to-etlas/master/dhall/prelude.dhall sha256:47a62403bcd28107cb0a99b05fc4d8bc73f06277c422d059ba9efdd50307abbc

let types =
      https://raw.githubusercontent.com/eta-lang/dhall-to-etlas/master/dhall/types.dhall sha256:d36b3384c6e0dc1837f5740b14eb20b1c51de1cb16a8008ee577d7d8f7ea115e

let v = prelude.v

let Haskell2010 = Some (types.Languages.Haskell2010 {=})

let pkg =
        λ(name : Text)
      → λ(version-range : types.VersionRange)
      → { bounds = version-range, package = name }

let pkgVer =
        λ(packageName : Text)
      → λ(minor : Text)
      → λ(major : Text)
      → pkg
        packageName
        ( prelude.intersectVersionRanges
          (prelude.orLaterVersion (v minor))
          (prelude.earlierVersion (v major))
        )

let deps =
      { base =
          pkgVer "base" "4.5" "5"
      , bytestring =
          pkgVer "bytestring" "0.10" "0.11"
      , contravariant =
          pkgVer "contravariant" "1.5" "1.6"
      , containers =
          pkgVer "containers" "0.5" "0.6"
      , cryptonite =
          pkgVer "cryptonite" "0.23" "1.0"
      , dhall =
          pkgVer "dhall" "1.19.1" "1.20"
      , dhall-eta=
          pkg "dhall-eta" prelude.anyVersion
      , directory =
          pkgVer "directory" "1.2.2.0" "1.4"
      , dotgen =
          pkgVer "dotgen" "0.4.2" "0.5"
      , eta-java-interop =
          pkgVer "eta-java-interop" "0.1.5.0" "0.1.6"
      , filepath =
          pkgVer "filepath" "1.4" "1.5"
      , megaparsec =
          pkgVer "megaparsec" "6.1.1" "7.1"
      , memory =
          pkgVer "memory" "0.14" "0.15"
      , lens =
          pkgVer "lens-family-core" "1.0.0" "1.3"
      , prettyprinter =
          pkgVer "prettyprinter" "1.2.0.1" "1.3"
      , scientific =
          pkgVer "scientific" "0.3.0.0" "0.4"
      , serialise =
          pkgVer "serialise" "0.2.0.0" "0.3"
      , tasty =
          pkgVer "tasty" "0.11.2" "1.2"
      , tasty-hunit =
          pkgVer "tasty-hunit" "0.9.2" "0.11"
      , text =
          pkgVer "text" "1.2" "1.3"
      , transformers =
          pkgVer "transformers" "0.2.0.0" "0.6"
      }

let warning-options =
      [ "-Wall"
      , "-fno-warn-safe"
      , "-fno-warn-unsafe"
      , "-fno-warn-implicit-prelude"
      , "-fno-warn-missing-import-lists"
      , "-fno-warn-missing-local-sigs"
      , "-fno-warn-monomorphism-restriction"
      , "-fno-warn-name-shadowing"
      ]

let extensions =
      [ types.Extension.DataKinds True
      , types.Extension.TypeOperators True
      , types.Extension.TypeFamilies True
      , types.Extension.FlexibleContexts True
      , types.Extension.MultiParamTypeClasses True
      , types.Extension.OverloadedStrings True
      ]

in    prelude.utils.GitHub-project { owner = "eta-lang", repo = "dhall-eta" }
    ⫽ { synopsis =
          "A wrapper of dhall-haskell to provide a friendly java api over dhall"
      , category =
          "Language"
      , build-type =
          Some (types.BuildType.Simple {=})
      , maintainer =
          "atreyu.bbb@gmail.com"
      , author =
          "Javier Neira Sánchez <atreyu.bbb@gmail.com>"
      , extra-source-files =
          [ "README.md" ]
      , license =
          types.License.BSD3 {=}
      , license-files =
          [ "LICENSE" ]
      , version =
          v "0.1.0"
      , cabal-version =
          v "1.12"
      , library =
          prelude.unconditional.library
          (   prelude.defaults.Library
            ⫽ { build-depends =
                  [ deps.base
                  , deps.bytestring
                  , deps.containers
                  , deps.contravariant
                  , deps.cryptonite
                  , deps.dhall
                  , deps.dotgen
                  , deps.eta-java-interop
                  , deps.megaparsec
                  , deps.memory
                  , deps.lens
                  , deps.prettyprinter
                  , deps.scientific
                  , deps.serialise
                  , deps.text
                  , deps.transformers
                  ]
              , compiler-options =
                  prelude.defaults.CompilerOptions ⫽ { GHC = warning-options }
              , exposed-modules =
                  [ "Dhall.Eta"
                  , "Dhall.Eta.Binary"
                  , "Dhall.Eta.Context"
                  , "Dhall.Eta.Core"
                  , "Dhall.Eta.Core.Java"
                  , "Dhall.Eta.Import"
                  , "Dhall.Eta.Parser"
                  , "Dhall.Eta.Parser.Java"
                  , "Dhall.Eta.TypeCheck"
                  , "Dhall.Eta.TypeCheck.Java"
                  , "Eta.Types"
                  ]
              , hs-source-dirs =
                  [ "src/main/eta" ]
              , java-sources =
                  [ "@classes.java" ]
              , default-extensions =
                  extensions
              , other-modules =
                  [ "Dhall.Eta.Map" ]
              , default-language =
                  Haskell2010
              }
          )
      , executables =
          [ prelude.unconditional.executable
            "dhall-eta-example"
            (   prelude.defaults.Executable
              ⫽ { build-depends =
                    [ deps.base
                    , deps.dhall-eta
                    ]
                , compiler-options =
                    prelude.defaults.CompilerOptions ⫽ { GHC = warning-options }
                , hs-source-dirs =
                    [ "examples/src/main/eta" ]
                , main-is =
                    "Main.hs"
                , default-extensions =
                    extensions
                , default-language =
                    Haskell2010
                }
            )
          ]
      , test-suites =
          [ prelude.unconditional.test-suite
            "tasty"
            (   prelude.defaults.TestSuite
              ⫽ { type =
                    types.TestType.exitcode-stdio { main-is = "Dhall/Eta/Test/Main.hs" }
                , build-depends =
                    [ deps.base
                    , deps.dhall
                    , deps.dhall-eta
                    , deps.directory
                    , deps.filepath
                    , deps.tasty
                    , deps.tasty-hunit
                    , deps.text
                    , deps.transformers
                    ]
                , compiler-options =
                    prelude.defaults.CompilerOptions ⫽ { GHC = warning-options }
                , default-extensions =
                    extensions
                , hs-source-dirs =
                    [ "src/test/eta"
                    ]
                , other-modules =
                    [ "Dhall.Eta.Test.Common"
                    , "Dhall.Eta.Test.Import"
                    , "Dhall.Eta.Test.Normalization"
                    , "Dhall.Eta.Test.Parser"
                    ]
                , default-language =
                    Haskell2010
                }
            )
          ]
      }
