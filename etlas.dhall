let prelude =
      https://raw.githubusercontent.com/eta-lang/dhall-to-etlas/etlas/dhall/prelude.dhall sha256:5ba8b5b12d40ba324909d3013c8a57cc71ddf6a8683eb7e2e453cc9338f67a0d

let types =
      https://raw.githubusercontent.com/eta-lang/dhall-to-etlas/etlas/dhall/types.dhall sha256:a6c967e2f3af97d621c2ec058822f41527e10d4288bd45b191e3a79f2ab87217

let idx =
      https://raw.githubusercontent.com/jneira/etlas-index/dhall-deps/dhall/index.dhall sha256:7c97a6d9f20bf2ee2710cff66cea98684a856476eda4402b5a339318d89cd9a9

let idx-types =
      https://raw.githubusercontent.com/jneira/etlas-index/dhall-deps/dhall/types/package.dhall
      
let 

let v = prelude.v

let map =
      https://raw.githubusercontent.com/dhall-lang/dhall-lang/master/Prelude/List/map

let idx-pkg
        : idx-types.IndexedPackage → types.Dependency
        =   λ(pkg : IndexedPackage)
          → { package = pkg.name
            , bounds = prelude.thisVersion (v pkg.lastest)
            }

let idx-deps =
      map idx-types.IndeexedPackage types.Dependency idx-pkg idx

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
      idx-deps.{ base, bytestring, contravariant, containers, cryptonite, eta-java-interop, filepath, megaparsec, memory, scientific, serialise, tasty, text, transformers}
      ⫽  { base =
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
      , dhall-eta =
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
