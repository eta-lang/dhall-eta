let prelude =
      https://raw.githubusercontent.com/eta-lang/dhall-to-etlas/etlas/dhall/prelude.dhall sha256:9ae883c3a3691ed57945635e5058979d0523d659d74afd814bd27c93ba05b830

let types =
      https://raw.githubusercontent.com/eta-lang/dhall-to-etlas/etlas/dhall/types.dhall sha256:0f312f257aa904a1d9b4065a630783c1544cb8ebe8f30e3efb1899b5e05625f1

let deps =
      https://raw.githubusercontent.com/eta-lang/dhall-to-etlas/etlas/dhall/dependencies.dhall sha256:150822c3da46c5a2a624445ad897b266826401a12ac588bd36cc84356a4cbd6d

let v = prelude.v

let dep = prelude.Dependency.singleInterval

let any = prelude.Dependency.any

let projectName = "dhall-eta"

let project =
      prelude.utils.GitHubTag-project
      { owner = "eta-lang", repo = projectName, version = "1.0.0" }

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
                    , deps.eta-java-interop
                    , deps.megaparsec
                    , deps.memory
                    , deps.scientific
                    , deps.serialise
                    , deps.text
                    , deps.transformers
                    ]
                  # [ dep "dotgen" "[0.4.2,0.5)"
                    , dep "lens-family-core" "[1.0.0,1.3)"
                    , dep "prettyprinter" "[1.2.0.1,1.3)"
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
                    [ deps.base, any projectName ]
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
                      , deps.directory
                      , deps.filepath
                      , deps.tasty
                      , deps.text
                      , deps.transformers
                      ]
                    # [ any projectName
                      , dep "tasty-hunit" "[0.9.2,0.11)"
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
