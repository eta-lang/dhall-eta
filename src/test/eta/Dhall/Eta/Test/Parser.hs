module Dhall.Eta.Test.Parser where

import qualified Dhall.Parser as Dhall
import qualified Dhall.Eta.Parser as Dhall.Eta

import Data.Text ( Text )
import Dhall.Eta.Parser.Java
import Dhall.Eta.Core.Java
import System.FilePath (takeBaseName)

import Test.Tasty
import Test.Tasty.HUnit

import Eta.Types
import Java

tests :: ([(FilePath, Text)], [(FilePath, Text)]) -> TestTree
tests (shouldParseCases, shouldNotParseCases) =
  testGroup "Parser tests"
    [ testGroup "Should parse"
      ( map shouldParseAndBeEqual shouldParseCases )
    , testGroup "Should NOT parse"
      ( map shouldNotParse shouldNotParseCases )
    ]

shouldNotParse :: (FilePath, Text) -> TestTree
shouldNotParse = parseShouldBeEqual succeded failed
  where succeded = fail "The parsing succeded but it had to fail"
        failed = return ()
          
shouldParseAndBeEqual :: (FilePath, Text) -> TestTree
shouldParseAndBeEqual = parseShouldBeEqual succeded failed
  where succeded = return ()
        failed = fail "The parsing failed but it had to success."
        
parseShouldBeEqual :: Assertion -> Assertion -> (FilePath, Text) -> TestTree
parseShouldBeEqual successAssert errorAssert ( path, txt ) =
  testCase ( takeBaseName path )
  ( do
      let eExpr = Dhall.exprFromText "" txt
          ejExpr = Dhall.Eta.exprFromText "" ( toJava txt )
          isJParseSuccess =
            ejExpr `instanceOf` getClass (Proxy :: Proxy (JRight a b))
          jexpr =
            jrightValue $ ((unsafeCast ejExpr)
                            :: JRight JParseError (JExpr JSrc JImport))
      case (eExpr, isJParseSuccess) of
        (Left  _,    False) ->
          errorAssert
        (Left  _,    True)  ->
          fail "Dhall parsing failed but Dhall.Eta one didn't."
        (Right _,    False) ->
          fail "Dhall parsing succeded but Dhall.Eta one didn't."
        (Right expr, True)  ->
          successAssert >>
          assertEqual
            ( "Parsing is not equal between Dhall and Dhall.Eta." )
            expr ( fromJava jexpr )
  )

