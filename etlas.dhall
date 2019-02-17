let prelude =
      https://raw.githubusercontent.com/eta-lang/dhall-to-etlas/etlas/dhall/prelude.dhall sha256:45889a631c526bca2c0a91b9c6d1f815d25f77d6a6785b4f6bdec40bcee6b3a0

let types =
      https://raw.githubusercontent.com/eta-lang/dhall-to-etlas/etlas/dhall/types.dhall sha256:a6c967e2f3af97d621c2ec058822f41527e10d4288bd45b191e3a79f2ab87217

let default-deps =
      https://raw.githubusercontent.com/eta-lang/dhall-to-etlas/etlas/dhall/dependencies.dhall sha256:e034fad0030e42d5fb1d21056ced132eaf9ded8adcd84f70bc8eaed0e60b1bfe

let v = prelude.v

let dep = prelude.Dependency.cons

let pvp = prelude.Dependency.pvp

let deps =
        default-deps.{ base
                     , bytestring
                     , contravariant
                     , containers
                     , cryptonite
                     , dhall
                     , directory
                     , eta-java-interop
                     , filepath
                     , megaparsec
                     , memory
                     , scientific
                     , serialise
                     , tasty
                     , text
                     , transformers
                     }
      ⫽ { dhall-eta =
            dep "dhall-eta" prelude.anyVersion
        , dotgen =
            pvp "dotgen" "0.4.2" "0.5"
        , lens =
            pvp "lens-family-core" "1.0.0" "1.3"
        , prettyprinter =
            pvp "prettyprinter" "1.2.0.1" "1.3"
        , tasty-hunit =
            pvp "tasty-hunit" "0.9.2" "0.11"
        }

let project =
      prelude.utils.GitHubTag-project
      { owner = "eta-lang", repo = "dhall-eta", version = "1.0.0" }

in    project
    ⫽ { synopsis =
          "dhall-eta is a eta library that wraps the haskell implementation of dhall configuration language."
      , description =
          ""
      , category =
          "Language"
      , maintainer =
          "atreyu.bbb@gmail.com"
      , author =
          "Javier Neira Sánchez <atreyu.bbb@gmail.com>"
      , extra-source-files =
          [ "build.gradle"
          , "dhall-eta.cabal"
          , "dhall-eta.dhall"
          , "examples/build.gradle"
          , "examples/src/main/java/org/dhall/eta/example/*.java"
          , "gradlew"
          , "gradlew.bat"
          , "gradle/wrapper/gradle-wrapper.jar"
          , "gradle/wrapper/gradle-wrapper.properties"
          , "java/build.gradle"
          , "java/src/main/java/org/dhall/*.java"
          , "java/src/main/java/org/dhall/binary/*.java"
          , "java/src/main/java/org/dhall/binary/decoding/failure/*.java"
          , "java/src/main/java/org/dhall/common/types/*.java"
          , "java/src/main/java/org/dhall/common/types/either/*.java"
          , "java/src/main/java/org/dhall/common/types/functor/*.java"
          , "java/src/main/java/org/dhall/core/*.java"
          , "java/src/main/java/org/dhall/core/constant/*.java"
          , "java/src/main/java/org/dhall/core/expr/*.java"
          , "java/src/main/java/org/dhall/core/imports/*.java"
          , "java/src/main/java/org/dhall/core/imports/hashed/*.java"
          , "java/src/main/java/org/dhall/core/imports/types/*.java"
          , "java/src/main/java/org/dhall/core/imports/types/url/*.java"
          , "proguard.txt"
          , "README.md"
          , "settings.gradle"
          , "src/main/java/org/dhall/eta/*.java"
          , "src/test/resources/import/data/foo/bar/*.dhall"
          , "src/test/resources/import/success/*.dhall"
          ]
      , license =
          types.License.BSD3 {=}
      , license-files =
          [ "LICENSE" ]
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
              , other-modules =
                  [ "Dhall.Eta.Map" ]
              }
          )
      , executables =
          [ prelude.unconditional.executable
            "dhall-eta-all"
            (   prelude.defaults.Executable
              ⫽ { build-depends =
                    [ deps.base, deps.dhall-eta ]
                , hs-source-dirs =
                    [ "examples/src/main/eta" ]
                , main-is =
                    "Main.hs"
                }
            )
          ]
      , test-suites =
          [ prelude.unconditional.test-suite
            "tasty"
            (   prelude.defaults.TestSuite
              ⫽ { type =
                    types.TestType.exitcode-stdio
                    { main-is = "Dhall/Eta/Test/Main.hs" }
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
                , hs-source-dirs =
                    [ "src/test/eta" ]
                , other-modules =
                    [ "Dhall.Eta.Test.Common"
                    , "Dhall.Eta.Test.Import"
                    , "Dhall.Eta.Test.Normalization"
                    , "Dhall.Eta.Test.Parser"
                    , "Dhall.Eta.Test.TypeCheck"
                    ]
                }
            )
          ]
      }
