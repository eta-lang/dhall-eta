module Dhall.Eta.Test.Normalization where

import qualified Dhall.Core as Dhall
import qualified Dhall.TypeCheck as Dhall

import qualified Dhall.Eta.Core as Dhall.Eta

import Data.Text ( Text )
import Dhall.Eta.Test.Common
import System.FilePath (takeDirectory, makeRelative, (</>))

import Test.Tasty
import Test.Tasty.HUnit

import Java

tests :: [(FilePath, Text)] -> TestTree
tests shouldNormalizeCases =
  testGroup "Normalizer tests"
    ( map shouldNormalizeAndBeEqual shouldNormalizeCases )
 

shouldNormalizeAndBeEqual :: (FilePath, Text) -> TestTree
shouldNormalizeAndBeEqual (path, txt) = testCase testName $ do
  exprs <- parseOrThrow txt
  (rExpr, jrExpr) <- resolveRelativeOrThrow (takeDirectory path) exprs
  let norm  = Dhall.alphaNormalize
             ( Dhall.normalize rExpr :: Dhall.Expr Dhall.X Dhall.X )
      jnorm = Dhall.Eta.alphaNormalizeResolved
              ( Dhall.Eta.normalizeResolved jrExpr )
  assertEqual
    ( "Normalization is not equal between Dhall and Dhall.Eta." )
    norm ( fromJava jnorm )
  where testName = makeRelative ( dhallCasesBasePath </> "normalization" )
                   path
  

