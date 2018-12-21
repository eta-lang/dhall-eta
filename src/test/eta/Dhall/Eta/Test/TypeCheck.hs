module Dhall.Eta.Test.TypeCheck where

import Data.Text ( Text )

import Dhall.Eta.Parser.Java
import Dhall.Eta.Core.Java
import Dhall.Eta.TypeCheck.Java

import Dhall.Eta.Test.Common

import System.FilePath (takeBaseName)

import Test.Tasty
import Test.Tasty.HUnit

import Eta.Types
import Java

tests :: ([(FilePath, Text)], [(FilePath,Text)])
      -> TestTree
tests (shouldTypeCheckCases, shouldNotTypeCheckCases) =
  testGroup "Import tests"
    [ testGroup "Should type check"
      ( map shouldTypeCheckAndBeEqual shouldTypeCheckCases )
    , testGroup "Should NOT type check"
      ( map shouldNotTypeCheck shouldNotTypeCheckCases )
    ]

shouldNotTypeCheck :: (FilePath, Text) -> TestTree
shouldNotTypeCheck = typeCheckShouldBeEqual succeded failed
  where succeded = fail "The type checking succeded but it had to fail"
        failed = return ()
          
shouldTypeCheckAndBeEqual :: (FilePath, Text) -> TestTree
shouldTypeCheckAndBeEqual = typeCheckShouldBeEqual succeded failed
  where succeded = return ()
        failed = fail "The type checking failed but it had to success."
        
typeCheckShouldBeEqual :: Assertion -> Assertion -> (FilePath, Text)
                       -> TestTree
typeCheckShouldBeEqual successAssert errorAssert ( path, txt ) =
  testCase ( "Type checking " ++ takeBaseName path )
  ( do
      (expr, jexpr) <- parseOrThrow txt
      (rexpr, rjexpr) <- resolveRelativeOrThrow path (expr, jexpr)
      let (texpr, tjexpr) = typeOf (rexpr, rjexpr)
          isJTypeCheckingSuccess =
            tjexpr `instanceOf` getClass (Proxy :: Proxy (JRight a b))
          jexpr =
            jrightValue $ ((unsafeCast tjexpr)
                           :: JRight (JTypeError JSrc JX) (JExpr JSrc JX))

      case (texpr, isJTypeCheckingSuccess) of
        (Left  _,    False) ->
          errorAssert
        (Left  _,    True)  ->
          fail "Dhall type checking failed but Dhall.Eta one didn't."
        (Right _,    False) ->
          fail "Dhall type checking succeded but Dhall.Eta one didn't."
        (Right expr, True)  ->
          successAssert >>
          assertEqual
            ( "Type checking is not equal between Dhall and Dhall.Eta." )
            expr ( fromJava jexpr )
  )


